---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:42.696159-07:00
description: "S\u1EEDa \u0111\u1ED5i t\u1EC7p t\u1EA1i ch\u1ED7 v\u1EDBi c\xE1c d\xF2\
  ng l\u1EC7nh CLI b\u1EB1ng PowerShell l\xE0 v\u1EC1 vi\u1EC7c th\u1EF1c hi\u1EC7\
  n ch\u1EC9nh s\u1EEDa tr\u1EF1c ti\u1EBFp c\xE1c t\u1EC7p t\u1EEB d\xF2ng l\u1EC7\
  nh, kh\xF4ng c\u1EA7n m\u1EDF ch\xFAng trong m\u1ED9t\u2026"
lastmod: '2024-03-13T22:44:36.930664-06:00'
model: gpt-4-0125-preview
summary: "S\u1EEDa \u0111\u1ED5i t\u1EC7p t\u1EA1i ch\u1ED7 v\u1EDBi c\xE1c d\xF2\
  ng l\u1EC7nh CLI b\u1EB1ng PowerShell l\xE0 v\u1EC1 vi\u1EC7c th\u1EF1c hi\u1EC7\
  n ch\u1EC9nh s\u1EEDa tr\u1EF1c ti\u1EBFp c\xE1c t\u1EC7p t\u1EEB d\xF2ng l\u1EC7\
  nh, kh\xF4ng c\u1EA7n m\u1EDF ch\xFAng trong m\u1ED9t\u2026"
title: "Ch\u1EC9nh s\u1EEDa file t\u1EA1i ch\u1ED7 v\u1EDBi c\xE2u l\u1EC7nh CLI ng\u1EAF\
  n g\u1ECDn"
weight: 32
---

## Làm gì & Tại sao?

Sửa đổi tệp tại chỗ với các dòng lệnh CLI bằng PowerShell là về việc thực hiện chỉnh sửa trực tiếp các tệp từ dòng lệnh, không cần mở chúng trong một trình biên tập. Phương pháp này giúp tiết kiệm thời gian và đặc biệt tiện lợi cho việc xử lý hàng loạt hoặc tự động hóa các nhiệm vụ chỉnh sửa lặp đi lặp lại trên nhiều tệp.

## Cách thực hiện:

### Thay thế Văn bản trong Một Tệp Đơn

Hãy bắt đầu với một nhiệm vụ đơn giản: bạn muốn thay thế tất cả các trường hợp của "oldtext" bằng "newtext" trong một tệp có tên là example.txt. Dưới đây là cách bạn thực hiện:

```PowerShell
(Get-Content example.txt) -replace 'oldtext', 'newtext' | Set-Content example.txt
```

Dòng lệnh này đọc nội dung, thực hiện việc thay thế, và viết nội dung trở lại tệp gốc.

### Chỉnh sửa Nhiều Tệp

Nếu bạn cần áp dụng cùng một thay đổi cho nhiều tệp thì sao? Dưới đây là một phương pháp sử dụng vòng lặp:

```PowerShell
Get-ChildItem *.txt | ForEach-Object {
  (Get-Content $_) -replace 'oldtext', 'newtext' | Set-Content $_
}
```

Đoạn mã này tìm tất cả các tệp `.txt` trong thư mục hiện tại, thay thế "oldtext" bằng "newtext" trong từng tệp.

### Thêm Nội dung vào Đầu hoặc Cuối các Tệp

Việc thêm hoặc đặt nội dung vào đầu hoặc cuối cũng có thể được tự động hóa:

```PowerShell
# Thêm vào đầu
"New first line`n" + (Get-Content example.txt) | Set-Content example.txt

# Thêm vào cuối
(Get-Content example.txt) + "`nNew last line" | Set-Content example.txt
```

Ở đây, chúng ta chỉ cần nối nội dung mới trước hoặc sau nội dung hiện tại và lưu lại.

## Sâu hơn nữa

Trong lịch sử, chỉnh sửa tại chỗ thường gắn liền với các công cụ Unix như `sed` và `awk`. PowerShell, là một công cụ mới hơn, không bao gồm tính năng chỉnh sửa tại chỗ ngay từ đầu. Điều này một phần là do triết lý thiết kế của nó, nhấn mạnh vào tầm quan trọng của các đối tượng hơn là luồng văn bản, không giống như các công cụ Unix coi hầu hết các đầu vào như văn bản.

Các phương án thay thế cho PowerShell cho nhiệm vụ này bao gồm việc sử dụng các công cụ Unix truyền thống có sẵn trên Windows thông qua Cygwin hoặc Hệ thống Phụ trợ Windows cho Linux (WSL). Những công cụ này thường cung cấp cú pháp ngắn gọn hơn cho việc chỉnh sửa tại chỗ do thiết kế tập trung vào văn bản của chúng.

Về mặt triển khai, quan trọng là phải lưu ý rằng cách tiếp cận của PowerShell bao gồm việc đọc toàn bộ tệp vào bộ nhớ, thực hiện thay đổi, rồi sau đó viết lại nó. Mặc dù điều này hoạt động tốt cho các tệp có kích thước vừa phải, nó có thể trở nên không hiệu quả cho các tệp rất lớn. Trong những trường hợp như vậy, có thể cần xem xét sử dụng trực tiếp các phương thức của `.NET` hoặc chuyển sang sử dụng các công cụ thay thế được thiết kế cho việc phát trực tiếp lớn lượng dữ liệu lớn.

Mặc dù có những cân nhắc này, sự linh hoạt và bộ tính năng rộng lớn của PowerShell làm cho nó trở thành một công cụ không thể thiếu để thao tác tệp trực tiếp từ dòng lệnh, đặc biệt là đối với những người đã gắn bó với hệ sinh thái Windows hoặc quản lý môi trường đa nền tảng.
