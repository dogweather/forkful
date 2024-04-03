---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:58.077370-07:00
description: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\
  \u1EDDng c\xF3 ngh\u0129a l\xE0 bi\u1EBFn \u0111\u1ED5i t\u1EA5t c\u1EA3 c\xE1c\
  \ k\xFD t\u1EF1 in hoa trong v\u0103n b\u1EA3n th\xE0nh c\xE1c k\xFD t\u1EF1 t\u01B0\
  \u01A1ng \u0111\u01B0\u01A1ng \u1EDF d\u1EA1ng ch\u1EEF th\u01B0\u1EDDng. L\u1EAD\
  p tr\xECnh\u2026"
lastmod: '2024-03-13T22:44:36.639859-06:00'
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\
  \u1EDDng c\xF3 ngh\u0129a l\xE0 bi\u1EBFn \u0111\u1ED5i t\u1EA5t c\u1EA3 c\xE1c\
  \ k\xFD t\u1EF1 in hoa trong v\u0103n b\u1EA3n th\xE0nh c\xE1c k\xFD t\u1EF1 t\u01B0\
  \u01A1ng \u0111\u01B0\u01A1ng \u1EDF d\u1EA1ng ch\u1EEF th\u01B0\u1EDDng."
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

## Làm thế nào:
Trong C#, bạn có thể chuyển đổi một chuỗi thành chữ thường sử dụng các phương thức `ToLower()` hoặc `ToLowerInvariant()`. Dưới đây là cách thực hiện:

```C#
string originalText = "Hello, World!";
string lowerCaseText = originalText.ToLower();

Console.WriteLine(lowerCaseText); // In ra: hello, world!
```

Và cho các phép chuyển đổi không phụ thuộc vào văn hóa:

```C#
string mixedCaseText = "İstanbul";
string lowerInvariantText = mixedCaseText.ToLowerInvariant();

Console.WriteLine(lowerInvariantText); // In ra: i̇stanbul
```

Kết quả mẫu:

```
hello, world!
i̇stanbul
```

## Đi Sâu Hơn
Về mặt lịch sử, nhu cầu chuyển đổi chuỗi thành chữ thường bắt nguồn từ các hệ thống máy tính bắt đầu với các lệnh không phân biệt chữ hoa chữ thường. Ngày nay, chúng ta vẫn làm điều này vì ba lý do chính:

1. **Nhất quán**: Khi xử lý đầu vào, đặc biệt là dữ liệu do người dùng tạo ra, chuyển đổi thành chữ thường đảm bảo một định dạng chuẩn.
2. **Các Thao Tác Không Phân Biệt Chữ Hoa Chữ Thường**: Bao gồm tìm kiếm, sắp xếp và so sánh các chuỗi, nơi "Apple" nên được xử lý giống như "apple".
3. **Địa phương hóa**: Các ngôn ngữ có các quy tắc khác nhau cho việc viết hoa. `ToLowerInvariant()` giải quyết vấn đề này bằng cách cung cấp một phép chuyển đổi không phụ thuộc vào văn hóa, chuyển các ký tự thành chữ thường dựa vào văn hóa bất biến (tương tự như tiếng Anh) và tránh kết quả không mong muốn.

Các phương án thay thế cho `.ToLower()` và `.ToLowerInvariant()` bao gồm sử dụng biểu thức chính quy để thay thế hoặc lặp qua một chuỗi một cách thủ công cho các kịch bản chuyển đổi tùy chỉnh.

Về chi tiết triển khai, những phương thức này không chỉnh sửa chuỗi gốc; chuỗi trong .NET là bất biến. Chúng tạo và trả về một chuỗi mới là phiên bản chữ thường của chuỗi gốc.

## Xem Thêm
- Lớp Chuỗi trong Tài liệu C# : [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string)
- Liệt kê StringComparison và So sánh Không Phụ Thuộc vào Văn Hóa: [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/base-types/best-practices-strings)
