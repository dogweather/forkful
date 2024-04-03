---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:40.719790-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: PowerShell l\xE0m cho vi\u1EC7c l\u1EA5\
  y \u0111\u1ED9 d\xE0i c\u1EE7a chu\u1ED7i tr\u1EDF n\xEAn \u0111\u01A1n gi\u1EA3\
  n. Ch\u1EC9 c\u1EA7n n\xE9m m\u1ED9t chu\u1ED7i v\xE0o thu\u1ED9c t\xEDnh `.Length`,\
  \ nh\u01B0 th\u1EBF n\xE0y."
lastmod: '2024-03-13T22:44:36.921432-06:00'
model: gpt-4-0125-preview
summary: "PowerShell l\xE0m cho vi\u1EC7c l\u1EA5y \u0111\u1ED9 d\xE0i c\u1EE7a chu\u1ED7\
  i tr\u1EDF n\xEAn \u0111\u01A1n gi\u1EA3n."
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

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
