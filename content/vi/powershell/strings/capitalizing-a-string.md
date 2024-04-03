---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:48.998599-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y l\xE0m cho m\u1ED9t s\u1ED1 v\u0103n\
  \ b\u1EA3n tr\u1EDF n\xEAn sinh \u0111\u1ED9ng h\u01A1n. Trong PowerShell, s\u1EED\
  \ d\u1EE5ng `.ToTitleCase` t\u1EEB `System.Globalization` cho vi\u1EC7c vi\u1EBF\
  t hoa ti\xEAu\u2026"
lastmod: '2024-03-13T22:44:36.910494-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y l\xE0m cho m\u1ED9t s\u1ED1 v\u0103n b\u1EA3n tr\u1EDF n\xEAn sinh\
  \ \u0111\u1ED9ng h\u01A1n."
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
weight: 2
---

## Làm thế nào:
Hãy làm cho một số văn bản trở nên sinh động hơn. Trong PowerShell, sử dụng `.ToTitleCase` từ `System.Globalization` cho việc viết hoa tiêu đề, hoặc các phương thức đơn giản như `.ToUpper()` hoặc `.ToLower()` để thay đổi trường hợp.

```PowerShell
# Tải lớp TextInfo để sử dụng ToTitleCase
$textInfo = (Get-Culture).TextInfo

# Ví dụ về trường hợp tiêu đề
$titleCaseString = $textInfo.ToTitleCase("hello, powershell aficionados!")
Write-Output $titleCaseString

# Đầu ra: Hello, Powershell Aficionados!

# Ví dụ về trường hợp chữ hoa
$upperCaseString = "make me shout".ToUpper()
Write-Output $upperCaseString

# Đầu ra: MAKE ME SHOUT

# Ví dụ về trường hợp chữ thường
$lowerCaseString = "SILENCE IS GOLDEN".ToLower()
Write-Output $lowerCaseString

# Đầu ra: silence is golden
```

## Sâu hơn nữa
Việc viết hoa bắt nguồn từ truyền thống chữ viết, nơi mà tiêu đề và danh từ riêng bắt đầu bằng chữ cái in hoa. Trong lập trình máy tính, thực hành này được áp dụng nhằm mục đích chuẩn hóa và dễ đọc hơn.

Về mặt kỹ thuật, `.ToTitleCase` không chỉ đơn thuần là việc làm cho các chữ cái viết hoa. Nó theo các quy tắc, như không viết hoa liên từ, giới từ, hoặc mạo từ trong một số ngữ cảnh. Bạn không mong đợi điều đó từ một đoạn code một dòng, phải không?

Có các phương án thay thế: regex có thể thực hiện những biến đổi trường hợp chữ cái phức tạp, nhưng nó quá mức cần thiết cho những công việc đơn giản. Hơn nữa, tính dễ đọc cũng quan trọng—`.ToTitleCase`, `.ToUpper()`, và `.ToLower()` cho bạn biết chính xác chúng làm gì. Không cần đoán mò.

Một chi tiết: hãy cẩn thận với các quy tắc cụ thể của văn hóa ảnh hưởng đến việc viết hoa. Ví dụ, "i" trở thành "I" trong tiếng Anh, nhưng không phải trong các ngôn ngữ khác. Đây là nơi `TextInfo` tỏa sáng; nó tôn trọng các nét tinh tế văn hóa.

## Xem Thêm
Hãy xem các tài nguyên này để hiểu sâu hơn:

- [Microsoft Docs về ToTitleCase](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase)
