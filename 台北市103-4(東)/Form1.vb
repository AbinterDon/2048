Public Class Form1
    '台北市103-4 吳東翰 2048
    Dim Box(4, 4) As Button
    Dim Point As Integer
    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Try
            FileOpen(1, "Point.txt", OpenMode.Input) '讀最高分數
            Input(1, Point) : Label2.Text = Point : FileClose()
        Catch ex As Exception

        End Try
        Me.KeyPreview = True '讓就算焦點在Button上面也可以接受鍵盤的事件
        For i As Integer = 1 To 4
            For x As Integer = 1 To 4
                Box(i, x) = Panel1.Controls("Button" & (i - 1) * 4 + x)
            Next
        Next
        SetNumber() : ColorIng()
    End Sub

    Protected Overrides Function ProcessDialogKey(ByVal keyData As Keys) As Boolean '讓key接受上下左右鍵
        If keyData = Keys.Left Or keyData = Keys.Right Or keyData = Keys.Up Or keyData = Keys.Down Then
            Return False
        Else
            Return MyBase.ProcessDialogKey(keyData)
        End If
    End Function

    Private Sub btRestart_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btRestart.Click '重新開始
        Beep()
        If MsgBox("這樣將會失去目前分數，確定要重新開始嗎?", 36) = vbYes Then Re() : SetNumber() : ColorIng() : Label1.Text = 0
    End Sub

    Private Sub Form1_KeyDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles MyBase.KeyDown '按下鍵盤任何一鍵的時候
        Dim cnt, ck As Integer
        If e.KeyCode = Keys.Up Or e.KeyCode = Keys.W Then '上
            For y = 1 To 4 'y
                For x = 2 To 4 'x
                    cnt = 0
                    Do While Box(x - cnt, y).Text <> ""
                        If Box(x - cnt - 1, y).Text = "" Then '如果下一格是空白的
                            Box(x - cnt - 1, y).Text = Box(x - cnt, y).Text
                            Box(x - cnt, y).Text = "" : ck = 1
                        ElseIf Box(x - cnt - 1, y).Text = Box(x - cnt, y).Text Then '如果下一格數字跟目前格子內的數字一樣
                            Box(x - cnt - 1, y).Text += Val(Box(x - cnt, y).Text) : ck = 1
                            Box(x - cnt, y).Text = "" : Label1.Text += Val(Box(x - cnt - 1, y).Text) : Exit Do '+分數 欄位內+總只能+一次
                        Else : Exit Do
                        End If
                        cnt += 1 : If x - cnt - 1 <= 0 Then Exit Do '判定下一格會不會超出範圍 會的話就跳出
                    Loop
                Next
            Next
            If ck = 1 Then SetNumber(ck) : ColorIng()
            Out()
        ElseIf e.KeyCode = Keys.Down Or e.KeyCode = Keys.S Then '下
            For y = 1 To 4 'y
                For x = 3 To 1 Step -1 'x
                    cnt = 0
                    Do While Box(x + cnt, y).Text <> ""
                        If Box(x + cnt + 1, y).Text = "" Then '如果下一格是空白的
                            Box(x + cnt + 1, y).Text = Box(x + cnt, y).Text
                            Box(x + cnt, y).Text = "" : ck = 1
                        ElseIf Box(x + cnt + 1, y).Text = Box(x + cnt, y).Text Then '如果下一格數字跟目前格子內的數字一樣
                            Box(x + cnt + 1, y).Text += Val(Box(x + cnt, y).Text) : ck = 1
                            Box(x + cnt, y).Text = "" : Label1.Text += Val(Box(x + cnt + 1, y).Text) : Exit Do '+分數 欄位內+總只能+一次
                        Else : Exit Do
                        End If
                        cnt += 1 : If x + cnt + 1 >= 5 Then Exit Do '判定下一格會不會超出範圍 會的話就跳出
                    Loop
                Next
            Next
            If ck = 1 Then SetNumber(ck) : ColorIng()
            Out()
        ElseIf e.KeyCode = Keys.Left Or e.KeyCode = Keys.A Then '左
            For x = 1 To 4 'y
                For y = 2 To 4 'x
                    cnt = 0
                    Do While Box(x, y - cnt).Text <> ""
                        If Box(x, y - cnt - 1).Text = "" Then '如果下一格是空白的
                            Box(x, y - cnt - 1).Text = Box(x, y - cnt).Text
                            Box(x, y - cnt).Text = "" : ck = 1
                        ElseIf Box(x, y - cnt - 1).Text = Box(x, y - cnt).Text Then '如果下一格數字跟目前格子內的數字一樣
                            Box(x, y - cnt - 1).Text += Val(Box(x, y - cnt).Text) : ck = 1
                            Box(x, y - cnt).Text = "" : Label1.Text += Val(Box(x, y - cnt - 1).Text) : Exit Do '+分數 欄位內+總只能+一次
                        Else : Exit Do
                        End If
                        cnt += 1 : If y - cnt - 1 <= 0 Then Exit Do '判定下一格會不會超出範圍 會的話就跳出
                    Loop
                Next
            Next
            If ck = 1 Then SetNumber(ck) : ColorIng()
            Out()
        ElseIf e.KeyCode = Keys.Right Or e.KeyCode = Keys.D Then '右
            For x = 1 To 4 'y
                For y = 3 To 1 Step -1 'x
                    cnt = 0
                    Do While Box(x, y + cnt).Text <> ""
                        If Box(x, y + cnt + 1).Text = "" Then '如果下一格是空白的
                            Box(x, y + cnt + 1).Text = Box(x, y + cnt).Text
                            Box(x, y + cnt).Text = "" : ck = 1
                        ElseIf Box(x, y + cnt + 1).Text = Box(x, y + cnt).Text Then '如果下一格數字跟目前格子內的數字一樣
                            Box(x, y + cnt + 1).Text += Val(Box(x, y + cnt).Text) : ck = 1
                            Box(x, y + cnt).Text = "" : Label1.Text += Val(Box(x, y + cnt + 1).Text) : Exit Do '+分數 欄位內+總只能+一次
                        Else : Exit Do
                        End If
                        cnt += 1 : If y + cnt + 1 >= 5 Then Exit Do '判定下一格會不會超出範圍 會的話就跳出
                    Loop
                Next
            Next
            If ck = 1 Then SetNumber(ck) : ColorIng()
            Out()
        End If
    End Sub

    Sub SetNumber(Optional ByVal ck As Integer = 0)
        If Out(ck) = 0 Then Exit Sub '當Game Over了
        Randomize()
        Dim n1, n2, Wd As Integer
        Do '隨機產生2或4
            n1 = Int((Rnd() * 4) + 1) : n2 = Int((Rnd() * 4) + 1) : Wd = Rnd() * 1
        Loop Until Box(n1, n2).Text = ""
        If Wd = 1 Then
            Box(n1, n2).Text = "4"
        Else : Box(n1, n2).Text = "2"
        End If
        'If Out() = 0 Then Exit Sub '當Game Over了
    End Sub

    Sub Re() '清空所有Button
        For i = 1 To 4
            For j = 1 To 4
                Box(i, j).Text = ""
            Next
        Next
    End Sub
    Sub ColorIng() '幫方塊塗顏色
        For x As Integer = 1 To 4
            For y As Integer = 1 To 4
                Box(x, y).BackColor = Color.White '所有格子背景色重製
                Select Case Box(x, y).Text
                    Case "2"
                        Box(x, y).BackColor = Color.FromArgb(238, 228, 128)
                    Case "4"
                        Box(x, y).BackColor = Color.FromArgb(236, 224, 200)
                    Case "8"
                        Box(x, y).BackColor = Color.FromArgb(242, 177, 112)
                    Case "16"
                        Box(x, y).BackColor = Color.FromArgb(236, 141, 83)
                    Case "32"
                        Box(x, y).BackColor = Color.FromArgb(245, 124, 95)
                    Case "64"
                        Box(x, y).BackColor = Color.FromArgb(233, 89, 55)
                    Case "128"
                        Box(x, y).BackColor = Color.FromArgb(243, 127, 107)
                    Case "256"
                        Box(x, y).BackColor = Color.FromArgb(241, 208, 75)
                    Case "512"
                        Box(x, y).BackColor = Color.FromArgb(228, 192, 42)
                    Case "1024"
                        Box(x, y).BackColor = Color.FromArgb(227, 186, 20)
                    Case "2048"
                        Box(x, y).BackColor = Color.FromArgb(227, 186, 20)
                    Case "4096"
                        Box(x, y).BackColor = Color.FromArgb(236, 196, 0)
                    Case "8192"
                        Box(x, y).BackColor = Color.FromArgb(60, 58, 50)
                End Select
            Next
        Next
    End Sub

    Function Out(Optional ByVal ck As Integer = 0) '測Game Over了沒
        For i = 1 To 4
            For j = 1 To 4
                If Box(i, j).Text = "" Then ck = 1 '當場面還有空格未填滿
                If i + 1 <= 4 Then If Box(i, j).Text = Box(i + 1, j).Text Then ck = 1 '當場面上還有可以相+的 x+1 y+1各判斷一次
                If j + 1 <= 4 Then If Box(i, j).Text = Box(i, j + 1).Text Then ck = 1
            Next
        Next
        If ck = 0 Then
            MsgBox("Game Over", , "結束") : Re() : SetNumber() : ColorIng()
            Try
                If Val(Label1.Text) > Point Then '如果比歷史分數高
                    FileOpen(1, "Point.txt", OpenMode.Output)
                    Point = Val(Label1.Text) : Label2.Text = Point
                    PrintLine(1, Val(Point) & "") : FileClose()
                    MsgBox("恭喜打破歷史紀錄", , "恭喜")
                End If
            Catch ex As Exception

            End Try
            Label1.Text = 0
        End If
        Return ck
    End Function
End Class
