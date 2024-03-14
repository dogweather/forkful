---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:09:21.835147-07:00
description: "Vi\u1EC7c vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra trong l\u1EADp tr\xEC\
  nh bao g\u1ED3m vi\u1EC7c t\u1EA1o ra c\xE1c th\u1EE7 t\u1EE5c c\u1EE5 th\u1EC3\
  \ \u0111\u1EC3 x\xE1c th\u1EF1c t\xEDnh n\u0103ng v\xE0 hi\u1EC7u su\u1EA5t c\u1EE7\
  a c\xE1c \u0111o\u1EA1n m\xE3 c\u1EE7a b\u1EA1n, \u0111\u1EA3m b\u1EA3o\u2026"
lastmod: '2024-03-13T22:44:36.437674-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra trong l\u1EADp tr\xECnh\
  \ bao g\u1ED3m vi\u1EC7c t\u1EA1o ra c\xE1c th\u1EE7 t\u1EE5c c\u1EE5 th\u1EC3 \u0111\
  \u1EC3 x\xE1c th\u1EF1c t\xEDnh n\u0103ng v\xE0 hi\u1EC7u su\u1EA5t c\u1EE7a c\xE1\
  c \u0111o\u1EA1n m\xE3 c\u1EE7a b\u1EA1n, \u0111\u1EA3m b\u1EA3o\u2026"
title: "Vi\u1EBFt ki\u1EC3m th\u1EED"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Việc viết các bài kiểm tra trong lập trình bao gồm việc tạo ra các thủ tục cụ thể để xác thực tính năng và hiệu suất của các đoạn mã của bạn, đảm bảo chúng hoạt động như mong đợi dưới các điều kiện khác nhau. Các lập trình viên làm điều này để phát hiện sớm các lỗi, cải thiện chất lượng mã và tạo điều kiện cho việc bảo trì và nâng cấp mã trong tương lai.

## Làm thế nào:

Dù Visual Basic for Applications (VBA) không đi kèm với một khuôn khổ kiểm thử tích hợp giống như những gì có sẵn trong các ngôn ngữ như Python hay JavaScript, bạn vẫn có thể thực hiện các thủ tục kiểm tra đơn giản để kiểm tra tính toàn vẹn của mã của bạn. Dưới đây là một ví dụ minh họa:

Giả sử bạn có một hàm trong VBA thực hiện việc cộng hai số:

```basic
Function AddNumbers(x As Integer, y As Integer) As Integer
    AddNumbers = x + y
End Function
```

Để kiểm tra hàm này, bạn có thể viết một thủ tục khác kiểm tra kết quả đầu ra so với kết quả mong đợi:

```basic
Sub TestAddNumbers()
    Dim result As Integer
    result = AddNumbers(5, 10)
    If result = 15 Then
        MsgBox "Kiểm tra thành công!", vbInformation
    Else
        MsgBox "Kiểm tra thất bại. Mong đợi 15 nhưng nhận được " & result, vbCritical
    End If
End Sub
```

Chạy `TestAddNumbers` sẽ hiển thị một hộp thông báo cho biết liệu bài kiểm tra đã vượt qua hay thất bại dựa trên kết quả đầu ra của hàm. Dù đây là một kịch bản đơn giản, bạn có thể xây dựng các bài kiểm tra phức tạp hơn bằng cách kết hợp vòng lặp, các giá trị đầu vào khác nhau và kiểm tra cho nhiều hàm.

## Sâu hơn

Cách tiếp cận viết kiểm tra trong VBA được trình bày ở đây là thủ công và thiếu các tính năng của các khuôn khổ kiểm thử tinh vi hơn có sẵn trong các môi trường lập trình khác, chẳng hạn như tự động chạy kiểm thử, thiết lập/tháo dỡ thủ tục, và tích hợp báo cáo kết quả kiểm thử. Trước sự chấp nhận rộng rãi của các khuôn khổ kiểm thử đơn vị và phát triển theo hướng kiểm thử (TDD), các thủ tục kiểm tra thủ công tương tự như đã mô tả là thông thường. Dù phương pháp này đơn giản và có thể hiệu quả cho các dự án nhỏ hoặc mục đích học tập, nó không phải là khả thi hoặc hiệu quả cho các dự án lớn hơn hoặc các nhóm làm việc.

Trong các môi trường hỗ trợ bộ công cụ phát triển phong phú hơn, các lập trình viên thường chuyển sang sử dụng các khuôn khổ như NUnit cho các ứng dụng .NET hoặc JUnit cho các ứng dụng Java, cung cấp các công cụ toàn diện để viết và chạy các bài kiểm tra một cách hệ thống. Các khuôn khổ này cung cấp các tính năng nâng cao như khẳng định kết quả kiểm tra, thiết lập đối tượng giả mạo, và đo lường độ bao phủ mã.

Đối với các nhà phát triển VBA tìm kiếm khả năng kiểm thử nâng cao hơn, lựa chọn gần nhất có thể là tận dụng các công cụ bên ngoài hoặc tích hợp với các môi trường lập trình khác. Một số nhà phát triển sử dụng VBA cùng với Excel để ghi lại các kịch bản kiểm tra và kết quả một cách thủ công. Dù không tiện lợi hay tự động như sử dụng một khuôn khổ kiểm thử chuyên dụng, những phương pháp này có thể một phần lấp đầy khoảng trống, giúp duy trì độ tin cậy của các giải pháp VBA trong các ứng dụng phức tạp hoặc quan trọng.
