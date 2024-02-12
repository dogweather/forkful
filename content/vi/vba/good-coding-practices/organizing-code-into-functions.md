---
title:                "Sắp xếp mã lập trình vào hàm số"
aliases:
- /vi/vba/organizing-code-into-functions.md
date:                  2024-02-01T21:57:15.426751-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã lập trình vào hàm số"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/vba/organizing-code-into-functions.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Việc tổ chức mã lệnh thành các hàm trong Visual Basic cho Ứng dụng (VBA) bao gồm việc chia nhỏ chương trình thành các phần nhỏ hơn, dễ quản lý hơn được biết đến là các hàm. Các lập trình viên làm điều này để tăng cường khả năng đọc mã, tái sử dụng mã hiệu quả, và đơn giản hóa quá trình gỡ lỗi và bảo trì.

## Làm thế nào:

Trong VBA, các hàm được định nghĩa sử dụng các câu lệnh `Function` và `End Function`. Dưới đây là một ví dụ đơn giản về cách tạo một hàm tính diện tích hình chữ nhật:

```basic
Function CalculateArea(length As Double, width As Double) As Double
    CalculateArea = length * width
End Function
```

Để gọi hàm này trong mã VBA của bạn và hiển thị kết quả trong hộp thông báo, bạn sẽ sử dụng:

```basic
Sub ShowArea()
    Dim area As Double
    area = CalculateArea(10, 5)
    MsgBox "Diện tích là " & area
End Sub
```

Khi thực thi, đoạn mã này hiển thị hộp thông báo nói rằng: `Diện tích là 50`.

### Truyền Biến ByRef và ByVal

VBA cho phép bạn truyền biến vào hàm hoặc theo tham chiếu (`ByRef`) hoặc theo giá trị (`ByVal`). Cách đầu tiên có nghĩa là biến gốc có thể được sửa đổi bởi hàm, trong khi cách sau truyền một bản sao, bảo vệ biến gốc khỏi những thay đổi.

```basic
Function ModifyValue(ByRef num As Integer)
    num = num + 5
End Function

Function PreserveValue(ByVal num As Integer) As Integer
    num = num + 5
    PreserveValue = num
End Function
```

## Sâu hơn nữa

VBA, là một ngôn ngữ lập trình điều khiển sự kiện, đặt trọng tâm đáng kể vào các hàm và subroutines để xử lý các nhiệm vụ khác nhau. Khác với nhiều ngôn ngữ hiện đại, VBA có một đặc điểm độc đáo khi từ khóa `Function` không chỉ khai báo một khối mã có thể tái sử dụng mà còn cho phép một giá trị trả về ngầm được trực tiếp gán cho tên của hàm.

Lịch sử, thiết kế của các hàm VBA đã được ảnh hưởng bởi các mô hình lập trình trước đó, nơi mà sự đóng gói và tính mô-đun dần được nhận ra là quan trọng trong phát triển phần mềm. Bối cảnh lịch sử này đã dẫn VBA đến việc áp dụng một cách tiếp cận cẩn trọng mà vẫn có chức năng để tổ chức mã.

Mặc dù VBA mạnh mẽ trong môi trường tự nhiên của nó (ví dụ như các ứng dụng Microsoft Office), quan trọng là phải lưu ý rằng thế giới lập trình đã phát triển. Các ngôn ngữ như Python cung cấp cú pháp đơn giản hơn và một thư viện tiêu chuẩn rộng lớn, làm cho chúng trở thành một lựa chọn ưu tiên cho các ứng dụng bên ngoài bộ Office. Tuy nhiên, khi làm việc trong các sản phẩm Microsoft Office, khả năng tích hợp và tự động hóa mà VBA cung cấp là không thể sánh kịp.

Đáng chú ý là, mặc dù tuổi tác, cộng đồng xung quanh VBA vẫn hoạt động, liên tục tìm ra các cách sáng tạo để khai thác chức năng của nó. Tuy nhiên, khi ngành công nghiệp phần mềm đang hướng tới các ngôn ngữ hiện đại, linh hoạt và mạnh mẽ hơn, các lập trình viên quen với VBA được khuyến khích khám phá những lựa chọn thay thế cho các công việc không liên quan đến Office để mở rộng bộ công cụ lập trình của họ.
