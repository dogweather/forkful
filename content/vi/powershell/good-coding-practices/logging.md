---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:02.806981-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 th\xF4\
  ng tin chi ti\u1EBFt v\u1EC1 c\xE1ch th\xEAm m\u1ED9t s\u1ED1 logging c\u01A1 b\u1EA3\
  n v\xE0o script c\u1EE7a b\u1EA1n."
lastmod: '2024-03-13T22:44:36.945070-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 th\xF4ng tin chi ti\u1EBFt v\u1EC1 c\xE1\
  ch th\xEAm m\u1ED9t s\u1ED1 logging c\u01A1 b\u1EA3n v\xE0o script c\u1EE7a b\u1EA1\
  n."
title: Ghi log
weight: 17
---

## Cách thực hiện:
Dưới đây là thông tin chi tiết về cách thêm một số logging cơ bản vào script của bạn:

```PowerShell
# Tạo một thông điệp log đơn giản
Write-Host "Info: Bắt đầu quá trình script."

# Ghi vào một file
"Info: Đây là một thông điệp log." | Out-File -Append myLog.log

# Sử dụng cmdlet có sẵn để log chi tiết hơn
Start-Transcript -Path "./detailedLog.log"
Write-Output "Cảnh báo: Có điều gì đó không ổn."
# ... script của bạn thực hiện công việc
Stop-Transcript

# Kết quả của detailedLog.log
******************************
Khởi đầu ghi chép Windows PowerShell 
Thời gian bắt đầu: 20230324112347
Tên người dùng: PShellGuru@example.com
Chạy với quyền người dùng: PShellGuru@example.com
Tên cấu hình: 
Máy: PS-DEVBOX (Microsoft Windows NT 10.0.17763.0)
Ứng dụng chủ: C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
ID quá trình: 2024
Phiên bản PS: 7.1.2
```

Bây giờ, trong log của bạn, có một báo cáo chi tiết về những gì code của bạn đã làm.

## Sâu hơn:
Lịch sử logging cũng cũ như chính lập trình. Nó giống như sổ log của thuyền trưởng nhưng dành cho phần mềm. Trước đây, có thể là in ấn hay máy đánh chữ; giờ đây tất cả là về file và hệ thống quản lý log phức tạp.

Khi bạn đang đào sâu vào PowerShell, `Write-Host` nhanh và dơ bẩn, nhưng chỉ đơn thuần là phun ra văn bản ra console, không tốt cho việc lưu trữ. `Out-File` cho bạn một cách đơn giản để đưa văn bản vào một file, nhưng để có được cái chất, bạn sẽ muốn sử dụng `Start-Transcript` và `Stop-Transcript` ghi lại mọi thứ—đầu vào, đầu ra, toàn bộ công việc.

Có sự lựa chọn khác không? Chắc chắn, nếu bạn đang triển khai ở quy mô doanh nghiệp, bạn có thể xem xét Windows Event Log hoặc sử dụng phần mềm như Logstash, nhưng cho việc script hàng ngày của bạn, hãy gắn bó với công cụ của PowerShell. Về việc triển khai, hãy nhớ log một cách thông minh – ít quá thì vô dụng, nhiều quá thì chỉ là tiếng ồn.

## Xem thêm:
Kiểm tra những điều này để nắm vững mọi thứ về logging trong PowerShell:
