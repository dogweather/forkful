---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:45.936174-07:00
description: "L\xE0m th\u1EBF n\xE0o: PowerShell l\xE0m cho vi\u1EC7c t\xECm ki\u1EBF\
  m v\xE0 thay th\u1EBF tr\u1EDF n\xEAn kh\xE1 \u0111\u01A1n gi\u1EA3n. H\xE3y xem\
  \ `-replace` cho chu\u1ED7i, v\xE0 `Get-Content` c\xF9ng v\u1EDBi `Set-Content`\
  \ cho\u2026"
lastmod: '2024-03-13T22:44:36.913440-06:00'
model: gpt-4-0125-preview
summary: "PowerShell l\xE0m cho vi\u1EC7c t\xECm ki\u1EBFm v\xE0 thay th\u1EBF tr\u1EDF\
  \ n\xEAn kh\xE1 \u0111\u01A1n gi\u1EA3n."
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
weight: 10
---

## Làm thế nào:
PowerShell làm cho việc tìm kiếm và thay thế trở nên khá đơn giản. Hãy xem `-replace` cho chuỗi, và `Get-Content` cùng với `Set-Content` cho tệp.

### Thay thế văn bản trong chuỗi:
```PowerShell
$text = "I love PowerShell"
$updatedText = $text -replace "love", "adore"
$updatedText
```
Kết quả mẫu:
```
I adore PowerShell
```

### Thay thế văn bản trong tệp:
```PowerShell
$file = "example.txt"
$content = Get-Content $file
$content | ForEach-Object { $_ -replace "oldWord", "newWord" } | Set-Content $file
```
Ở đây không có kết quả, nhưng `example.txt` giờ đã có mỗi "oldWord" được thay bằng "newWord".

## Sâu hơn
Từ khi chỉnh sửa văn bản ra đời, tìm kiếm và thay thế đã trở thành một trụ cột. Hãy nghĩ về nó như là chức năng tìm kiếm và thay thế trong trình xử lý từ nhưng được tăng cường cho nhu cầu lập trình.

Ngày xưa, những phù thủy dòng lệnh sử dụng công cụ như `sed` trong Unix. PowerShell đã đưa chức năng này vào ngôn ngữ kịch bản của mình. Tại sao nó cool? Bởi vì nó gắn liền với đối tượng, không chỉ là văn bản. Điều đó có nghĩa là bạn có thể điều chỉnh không chỉ mã và tệp văn bản mà còn cả cấu trúc dữ liệu và hơn thế nữa.

Các phương án khác? Chắc chắn rồi. Bạn có các trình soạn thảo văn bản và IDE với chức năng tìm và thay thế riêng của chúng, kịch bản hàng loạt, hoặc thậm chí là thư viện lập trình được thiết kế để thao tác với văn bản.

Chi tiết triển khai? PowerShell hỗ trợ regex. Điều đó có nghĩa là bạn có thể thay thế dựa trên các mẫu, không chỉ là từ cố định. Và, với kịch bản PowerShell, bạn có thể tự động hóa các thao tác này trên một số lượng lớn tệp, tiết kiệm cho bạn một lượng thời gian đáng kể.

## Xem thêm
- Tài liệu về toán tử `-replace` của PowerShell: [link](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators)
- Sử dụng `Get-Content` và `Set-Content`: [link](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
