---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:28.777368-07:00
description: "L\xE0m th\u1EBF n\xE0o: ."
lastmod: '2024-03-13T22:44:36.946326-06:00'
model: gpt-4-0125-preview
summary: .
title: "X\u1EED l\xFD l\u1ED7i"
weight: 16
---

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
