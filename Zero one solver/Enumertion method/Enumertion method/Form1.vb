'Option Strict On
Imports Z.Expressions

Public Class Form1
    Enum relations
        EQUAL '=
        LESS_THAN '<
        GREATER_THAN '>
        LESS_THAN_OR_EQUAL '<=
        GREATER_THAN_OR_EQUAL '>=
    End Enum

    Private Sub BtnSolve_Click(sender As Object, e As EventArgs) Handles BtnSolve.Click
        If Not rdioMax.Checked AndAlso Not rdioMin.Checked Then
            MessageBox.Show("Please select optimization type !", "Error !", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Exit Sub
        End If
        If txtCns.Text = "" Then
            MessageBox.Show("Please enter the constrains!", "Error !", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Exit Sub
        End If
        If txtObj.Text = "" Then
            MessageBox.Show("Please enter the object function !", "Error !", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Exit Sub
        End If

        txtObj.Text = txtObj.Text.ToLower
        txtCns.Text = txtCns.Text.ToLower

        GrdT.Columns.Clear()
        GrdT.Rows.Clear()
        GrdT.Update()
        GrdT.Refresh()

        Try
            solve()
        Catch ex As Exception
            MessageBox.Show("an error has occurred, please check your entries!", "Error !", MessageBoxButtons.OK, MessageBoxIcon.Error)
            MessageBox.Show(ex.Message)
        End Try
        TabControl1.SelectTab(TabPage2)
        TabControl1.Update()
        TabControl1.Refresh()
    End Sub
    Sub solve()

        Dim obj As String = txtObj.Text.Replace(" ", "")
        Dim vars As New List(Of Char)
        For Each xc As Char In obj
            If Char.IsLetter(xc) Then
                vars.Add(xc)
            End If
        Next
        GrdT.Columns.Add("clmSolution", "Solution")
        If Char.IsLetter(obj(0)) Then
            obj = obj.Insert(0, "1")
        End If
        For Each xc As Char In vars
            If Not (Char.IsNumber(obj(obj.IndexOf(xc) - 1))) Then
                obj = obj.Insert(obj.IndexOf(xc), "1")
            End If
            GrdT.Columns.Add("clm" & xc, xc)
        Next

        '//Begin Construct table
        Dim numDeVars As Integer = vars.Count '2
        Dim numDeLops As Integer = 2 ^ numDeVars '4
        Dim numDeHops As Integer = 2
        Dim Exponential As Integer = 0
        Dim numDeZeroz As Integer = 2 ^ (numDeVars - 1) '2
        Dim hopper As Integer = 0

        '///////////////////////////////////////
        For CntrDeRows = 0 To numDeLops - 1 Step 1 '0 1 2 3
            GrdT.Rows.Add()
            GrdT.Item("clmSolution", CntrDeRows).Value = CntrDeRows + 1
        Next
        '/////////////////////////////////////
        Dim x As Integer = 0
        Dim y As Integer = 0
        For columner = 1 To vars.Count  ' 1 2 3 
            x = 0
            y = 0
            hopper = 0
            numDeHops = 2 ^ Exponential
            Exponential += 1
            For Rower = 0 To numDeHops - 1 Step 1  ' 0  1

                For x = hopper To hopper + numDeZeroz - 1 ''0  1/4 5
                    GrdT.Item(columner, x).Value = 0
                Next
                hopper += numDeZeroz '2

                For y = x To hopper + numDeZeroz - 1 '2 - 3
                    GrdT.Item(columner, y).Value = 1
                Next
                hopper += numDeZeroz '4
            Next
            numDeZeroz /= 2

        Next
        GrdT.Columns.Add("Feasibility", "Feasibility")
        GrdT.Columns.Add("clmZObj", "Z Solution")
        GrdT.Update()
        GrdT.Refresh()
        '////////////////////
        'begin eval

        Dim cons As String = txtCns.Text.Replace(" ", "")

        Dim constrainsHolder As String() = cons.Split(Environment.NewLine)
        Dim ListOfRelations As New List(Of relations)
        Dim ListOfRights As New List(Of Double)
        Dim indexHolder As String = ""

        For T = 0 To constrainsHolder.Length - 1
            indexHolder = ""
            If Char.IsLetter(constrainsHolder(T)(0)) Then
                constrainsHolder(T) = constrainsHolder(T).Insert(0, "1")
            End If
            For Each xc As Char In vars
                If constrainsHolder(T).Contains(xc) AndAlso Not (Char.IsNumber(constrainsHolder(T)(constrainsHolder(T).IndexOf(xc) - 1))) Then
                    constrainsHolder(T) = constrainsHolder(T).Insert(constrainsHolder(T).IndexOf(xc), "1")
                End If
            Next
            If (constrainsHolder(T).Contains("<=") Or constrainsHolder(T).Contains("=<")) Then
                ListOfRelations.Add(relations.LESS_THAN_OR_EQUAL)
                indexHolder = "<="

            ElseIf (constrainsHolder(T).Contains(">=") Or constrainsHolder(T).Contains("=>")) Then
                ListOfRelations.Add(relations.GREATER_THAN_OR_EQUAL)
                indexHolder = ">="
            ElseIf (constrainsHolder(T).Contains("=")) Then
                ListOfRelations.Add(relations.EQUAL)
                indexHolder = "="

            ElseIf (constrainsHolder(T).Contains("<")) Then
                ListOfRelations.Add(relations.LESS_THAN)
                indexHolder = "<"

            ElseIf (constrainsHolder(T).Contains(">")) Then
                ListOfRelations.Add(relations.GREATER_THAN)
                indexHolder = ">"
            Else
            End If

            If indexHolder.Length = 2 Then
                ListOfRights.Add(constrainsHolder(T).Substring(constrainsHolder(T).IndexOf(indexHolder) + 2))
            Else
                ListOfRights.Add(constrainsHolder(T).Substring(constrainsHolder(T).IndexOf(indexHolder) + 1))
            End If
            constrainsHolder(T) = constrainsHolder(T).Substring(0, constrainsHolder(T).IndexOf(indexHolder))
        Next
        '///////////////////////////////////////
        Dim constrains As String()
        Dim obj2 As String = ""
        Dim evaledObj As Double = 0
        Dim evaledCons As Double = 0
        Dim feasibleWeight As Integer = 0
        Dim feasibles As New List(Of Double)
        For rowI As Integer = 0 To (2 ^ vars.Count) - 1 Step 1 '0 .. 7
            'rower
            obj2 = obj
            constrains = constrainsHolder.Clone()
            '///////////////////////////////////////////////////////
            For columnI As Integer = 1 To vars.Count Step 1 ' 0 1 2
                'colum
                obj2 = obj2.Replace(vars(columnI - 1), "*" & GrdT.Item(columnI, rowI).Value) '2*0+4*1 
                '/////////////////////////////////////
                For cnsone As Integer = 0 To constrains.Length - 1 ' 0
                    constrains(cnsone) = constrains(cnsone).Replace(vars(columnI - 1), "*" & GrdT.Item(columnI, rowI).Value)
                Next
                '//////////////////////////////////////
            Next
            'rower
            feasibleWeight = 0
            For cnsone = 0 To constrains.Length - 1
                evaledCons = Eval.Execute(constrains(cnsone))
                If ListOfRelations(cnsone) = relations.GREATER_THAN_OR_EQUAL Then
                    If evaledCons >= ListOfRights(cnsone) Then
                        feasibleWeight += 1
                    End If
                ElseIf ListOfRelations(cnsone) = relations.LESS_THAN_OR_EQUAL Then
                    If evaledCons <= ListOfRights(cnsone) Then
                        feasibleWeight += 1
                    End If

                ElseIf ListOfRelations(cnsone) = relations.GREATER_THAN Then
                    If evaledCons > ListOfRights(cnsone) Then
                        feasibleWeight += 1
                    End If
                ElseIf ListOfRelations(cnsone) = relations.LESS_THAN Then
                    If evaledCons < ListOfRights(cnsone) Then
                        feasibleWeight += 1
                    End If
                ElseIf ListOfRelations(cnsone) = relations.EQUAL Then
                    If evaledCons = ListOfRights(cnsone) Then
                        feasibleWeight += 1
                    End If
                End If
            Next
            If feasibleWeight = constrains.Length Then
                evaledObj = Eval.Execute(obj2)
                GrdT.Item("Feasibility", rowI).Value = "Feasible"
                GrdT.Item("clmZObj", rowI).Value = evaledObj
                feasibles.Add(evaledObj)
            Else
                GrdT.Item("Feasibility", rowI).Value = "Infeasible"
                GrdT.Item("clmZObj", rowI).Value = Double.PositiveInfinity
            End If
        Next
        Dim feasibleSolution As Double
        If rdioMax.Checked Then
            feasibleSolution = feasibles.Max
        ElseIf rdioMin.Checked Then
            feasibleSolution = feasibles.Min
        End If
        For YY = 0 To numDeLops - 1
            If GrdT.Item("clmZObj", YY).Value = feasibleSolution Then
                GrdT.Item("clmZObj", YY).Value = GrdT.Item("clmZObj", YY).Value & " *"
                With GrdT.Rows(YY).DefaultCellStyle
                    .ForeColor = Color.Black
                    .BackColor = Color.Yellow
                End With
            End If
        Next
    End Sub

End Class
