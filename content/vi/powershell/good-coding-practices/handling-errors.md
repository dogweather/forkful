---
aliases:
- /vi/powershell/handling-errors/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:28.777368-07:00
description: "X\u1EED l\xFD l\u1ED7i trong PowerShell c\xF3 ngh\u0129a l\xE0 d\u1EF1\
  \ \u0111o\xE1n nh\u1EEFng s\u1EF1 c\u1ED1 v\xE0 qu\u1EA3n l\xFD ch\xFAng m\u1ED9\
  t c\xE1ch m\u01B0\u1EE3t m\xE0. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u\
  \ n\xE0y \u0111\u1EC3 ng\u0103n ch\u1EB7n s\u1EF1 c\u1ED1 v\xE0 cung c\u1EA5p\u2026"
lastmod: 2024-02-18 23:08:50.942459
model: gpt-4-0125-preview
summary: "X\u1EED l\xFD l\u1ED7i trong PowerShell c\xF3 ngh\u0129a l\xE0 d\u1EF1 \u0111\
  o\xE1n nh\u1EEFng s\u1EF1 c\u1ED1 v\xE0 qu\u1EA3n l\xFD ch\xFAng m\u1ED9t c\xE1\
  ch m\u01B0\u1EE3t m\xE0. L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y\
  \ \u0111\u1EC3 ng\u0103n ch\u1EB7n s\u1EF1 c\u1ED1 v\xE0 cung c\u1EA5p\u2026"
title: "X\u1EED l\xFD l\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Xử lý lỗi trong PowerShell có nghĩa là dự đoán những sự cố và quản lý chúng một cách mượt mà. Lập trình viên làm điều này để ngăn chặn sự cố và cung cấp phản hồi hữu ích cho người dùng.

## Làm thế nào:
```PowerShell
# Cơ bản về Try-Catch để xử lý ngoại lệ
try {
    # Mã có thể gây ra lỗi
    $result = 1 / 0
} catch {
    # Làm gì nếu xảy ra lỗi
    Write-Host "Ồ, đã xảy ra lỗi: $_"
}

# Xuất thông báo lỗi tùy chỉnh
try {
    Get-Item "nonexistentfile.txt" -ErrorAction Stop
} catch {
    Write-Host "Không thể tìm thấy tệp."
}

# Sử dụng biến $Error để kiểm tra lỗi cuối cùng
```
## Sâu hơn nữa
PowerShell đã đi một chặng đường dài kể từ khi nó ra đời dưới tên Monad. Việc xử lý lỗi trở nên mạnh mẽ hơn theo thời gian, mang lại những tính năng tương tự như trong các ngôn ngữ lập trình khác. Cú pháp `try-catch-finally` là một ví dụ về việc chia sẻ từ các ngôn ngữ như C#. Trước đó, những người viết kịch bản phải dựa nhiều vào việc kiểm tra điều kiện và sử dụng biến tự động `$Error`.

PowerShell cũng có hai loại lỗi chính: lỗi dừng và lỗi không dừng. Lỗi dừng sẽ dừng kịch bản trừ khi được bắt trong một khối `try-catch`, trong khi lỗi không dừng sẽ không dừng lại trừ khi bạn chỉ định `-ErrorAction Stop`. Sự phân biệt này rất quan trọng vì nó cấp quyền kiểm soát chặt chẽ về xử lý lỗi, quyết định liệu một lỗi có thực sự xứng đáng dừng toàn bộ kịch bản hay có thể đơn giản được ghi lại và bỏ qua.

Xử lý lỗi trong PowerShell cũng cho phép sử dụng khối `finally`, chạy bất kể điều gì xảy ra - cho dù có lỗi hay không. Điều này tuyệt vời cho các tác vụ dọn dẹp.

Khi bạn đang sâu trong việc viết kịch bản, bạn cũng có thể xử lý các loại ngoại lệ cụ thể, mang lại sự kiểm soát còn tinh tế hơn.

Ngoài ra, còn có cách truyền thống sử dụng tham số `-ErrorVariable` để ghi lại lỗi mà không phải ném ra ngoại lệ. Và biến `$?` cho bạn biết liệu thao tác cuối cùng có thành công hay không. Chúng là những công cụ hữu ích, mặc dù hơi kém gọn gàng hơn một khối `try-catch` chắc chắn.

## Xem thêm
- [about_Try_Catch_Finally](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_try_catch_finally?view=powershell-7.2)
