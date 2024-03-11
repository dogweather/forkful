---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:40.719790-07:00
description: "Trong PowerShell, vi\u1EC7c t\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9\
  t chu\u1ED7i ngh\u0129a l\xE0 \u0111\u1EBFm s\u1ED1 l\u01B0\u1EE3ng k\xFD t\u1EF1\
  \ m\xE0 n\xF3 ch\u1EE9a. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7\
  c n\xE0y \u0111\u1EC3 x\xE1c nh\u1EADn \u0111\u1EA7u v\xE0o, thao\u2026"
lastmod: '2024-03-11T00:14:10.214244-06:00'
model: gpt-4-0125-preview
summary: "Trong PowerShell, vi\u1EC7c t\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9\
  t chu\u1ED7i ngh\u0129a l\xE0 \u0111\u1EBFm s\u1ED1 l\u01B0\u1EE3ng k\xFD t\u1EF1\
  \ m\xE0 n\xF3 ch\u1EE9a. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7\
  c n\xE0y \u0111\u1EC3 x\xE1c nh\u1EADn \u0111\u1EA7u v\xE0o, thao\u2026"
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Trong PowerShell, việc tìm độ dài của một chuỗi nghĩa là đếm số lượng ký tự mà nó chứa. Các lập trình viên thực hiện việc này để xác nhận đầu vào, thao tác dữ liệu văn bản, và đảm bảo dữ liệu phù hợp với các tiêu chí hoặc định dạng nhất định.

## Cách thực hiện:
PowerShell làm cho việc lấy độ dài của chuỗi trở nên đơn giản. Chỉ cần ném một chuỗi vào thuộc tính `.Length`, như thế này:

```PowerShell
$myString = "Hello, World!"
$myStringLength = $myString.Length
Write-Host "Độ dài của chuỗi là: $myStringLength"
```

Bạn sẽ nhận được đầu ra:

```
Độ dài của chuỗi là: 13
```

Chỉ có vậy thôi. Trực tiếp và không đau đớn.

## Sâu hơn một chút
Trước đây, việc lấy độ dài của một chuỗi trong hầu hết các ngôn ngữ lập trình đòi hỏi các hàm hoặc quy trình phức tạp. Ngày nay, nó đơn giản như một lời gọi thuộc tính trong PowerShell.

Ngoài thuộc tính `.Length` cơ bản, PowerShell không cung cấp các lựa chọn thay thế tích hợp sẵn cho nhiệm vụ cụ thể này. Tuy nhiên, trước khi PowerShell trở thành một thực thể, việc kịch bản hóa trong Windows được thực hiện qua tệp lô hoặc VBScript, nơi việc tìm độ dài của chuỗi không trực tiếp như vậy.

Về mặt triển khai, khi bạn sử dụng `$myString.Length`, PowerShell truy cập vào metadata của đối tượng chuỗi – chuỗi trong PowerShell là các đối tượng từ lớp System.String, đến từ .NET. Thuộc tính `.Length` là một thành viên của lớp đó.

## Xem thêm
Khám phá sâu hơn về chuỗi trong PowerShell:

Để hiểu rộng hơn về cách chuỗi hoạt động trong .NET:
- [Lớp Chuỗi trong .NET](https://docs.microsoft.com/dotnet/api/system.string)
- [Thuộc tính String.Length trong .NET](https://docs.microsoft.com/dotnet/api/system.string.length)
