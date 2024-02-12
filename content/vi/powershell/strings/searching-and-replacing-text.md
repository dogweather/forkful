---
title:                "Tìm kiếm và thay thế văn bản"
aliases:
- /vi/powershell/searching-and-replacing-text.md
date:                  2024-01-28T22:07:45.936174-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tìm kiếm và thay thế văn bản trong các tệp: nó là hoán đổi từ hoặc cụm từ này bằng từ hoặc cụm từ khác. Lập trình viên sử dụng nó để cập nhật mã, sửa lỗi, hoặc thay đổi dữ liệu trên nhiều tệp một cách nhanh chóng mà không cần phải thủ công kiểm tra từng tệp.

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
