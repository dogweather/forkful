---
title:                "Kiểm tra nếu một thư mục tồn tại"
aliases:
- /vi/vba/checking-if-a-directory-exists/
date:                  2024-02-01T21:49:58.105441-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kiểm tra nếu một thư mục tồn tại"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/vba/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Kiểm tra xem một thư mục có tồn tại trong Visual Basic cho Ứng dụng (VBA) là để xác minh sự hiện diện của một thư mục trong hệ thống tập tin trước khi thực hiện các thao tác như lưu tệp hoặc tạo thư mục mới. Các lập trình viên thực hiện điều này để tránh lỗi thời gian chạy và đảm bảo mã của họ tương tác với hệ thống tập tin một cách hiệu quả và chính xác.

## Làm thế nào:

Trong VBA, để kiểm tra xem một thư mục có tồn tại không, bạn thường sử dụng hàm `Dir` kết hợp với thuộc tính `vbDirectory`. Phương pháp này cho phép bạn kiểm tra sự tồn tại của một thư mục bằng cách chỉ rõ đường dẫn của nó. Đây là cách bạn có thể làm:

```basic
Dim folderPath As String
folderPath = "C:\TestFolder"

If Dir(folderPath, vbDirectory) = "" Then
    MsgBox "Thư mục không tồn tại.", vbExclamation
Else
    MsgBox "Thư mục tồn tại.", vbInformation
End If
```

Đoạn mã trên đầu tiên định nghĩa một đường dẫn thư mục (`C:\TestFolder`). Hàm `Dir` sau đó cố gắng tìm thư mục này sử dụng thuộc tính `vbDirectory`. Nếu thư mục không tồn tại, `Dir` sẽ trả về một chuỗi trống, và chúng ta hiển thị một hộp thông báo cho biết thư mục không tồn tại. Ngược lại, chúng ta hiển thị một thông báo khác cho biết thư mục tồn tại.

Kết quả mẫu khi thư mục không tồn tại:
```
Thư mục không tồn tại.
```

Kết quả mẫu khi thư mục tồn tại:
```
Thư mục tồn tại.
```

## Sâu hơn nữa

Việc kiểm tra xem một thư mục có tồn tại không là một nhiệm vụ cơ bản trong nhiều ngôn ngữ lập trình, không chỉ trong VBA. Phương pháp sử dụng `Dir` được mô tả ở trên là đơn giản và hiệu quả cho hầu hết các mục đích trong VBA. Tuy nhiên, cần lưu ý rằng cách tiếp cận này có thể có hạn chế, chẳng hạn như trong trường hợp của đường dẫn mạng và xử lý quyền, có thể đôi khi mang lại kết quả sai lệch.

Theo lịch sử, các phương pháp truy cập hệ thống tập tin đã phát triển qua các ngôn ngữ lập trình khác nhau, với những ngôn ngữ gần đây hơn cung cấp các cách tiếp cận hướng đối tượng. Ví dụ, trong các ngôn ngữ .NET như VB.NET, người ta có thể sử dụng `System.IO.Directory.Exists(path)` cho một cách thức kiểm tra tồn tại thư mục dễ dàng và có thể nói là mạnh mẽ hơn, tận dụng việc xử lý ngoại lệ và thông tin trả về phong phú hơn.

Mặc dù VBA không có các lớp tích hợp sẵn mạnh mẽ như những cái được tìm thấy trong .NET cho các thao tác hệ thống tập tin, nhưng hiểu được tiện ích và hạn chế của hàm `Dir` là quan trọng để viết các kịch bản VBA hiệu quả tương tác với hệ thống tập tin. Trong các tình huống mà khả năng của VBA không đủ, việc tích hợp các thành phần .NET hoặc tận dụng các kịch bản bên ngoài có thể cung cấp những lựa chọn tốt hơn.
