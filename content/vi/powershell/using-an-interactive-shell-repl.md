---
title:                "Sử dụng vỏ tương tác (REPL)"
date:                  2024-01-28T22:09:46.939396-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"

category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
Shell tương tác, hay Vòng lặp Đọc-Đánh giá-In (REPL), cho phép bạn nhập các lệnh PowerShell và nhận phản hồi tức thì. Lập trình viên sử dụng nó để nhanh chóng kiểm tra các đoạn mã, gỡ lỗi, hoặc học các lệnh mới mà không cần viết một script đầy đủ.

## Làm thế nào:
Khởi chạy PowerShell và bạn sẽ ở trong REPL. Thử lệnh Cmdlet `Get-Date`:

```PowerShell
PS > Get-Date
```

Bạn sẽ thấy đầu ra là ngày và giờ hiện tại:

```PowerShell
Thứ Tư, Ngày 31 tháng Ba, 2023 12:34:56 CH
```

Bây giờ, kết hợp các lệnh. Hãy sắp xếp các quy trình theo sử dụng bộ nhớ:

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

Điều này hiển thị 5 quy trình hàng đầu theo kích thước bộ làm việc (sử dụng bộ nhớ).

## Sâu hơn
REPL của PowerShell có nguồn gốc từ shell Unix và các shell ngôn ngữ động khác như Python. Đó là một môi trường thực hiện lệnh tương tác đơn người. Khác với một ngôn ngữ biên dịch, nơi bạn viết toàn bộ ứng dụng rồi mới biên dịch, môi trường REPL cho phép bạn viết và chạy mã từng dòng một lúc. PowerShell cũng hỗ trợ thực thi script cho các nhiệm vụ lớn hơn.

Các lựa chọn khác cho Windows bao gồm Command Prompt hoặc các REPL đặc thù cho ngôn ngữ khác như IPython. Trong thế giới Unix/Linux, các shell như bash hay zsh thực hiện chức năng tương tự.

Thực thi của PowerShell sử dụng một ứng dụng chủ để chạy shell. Trong khi PowerShell.exe trong Windows là phổ biến nhất, các tùy chọn khác như Môi trường Scripting Tích hợp (ISE) hoặc terminal tích hợp của Visual Studio Code cũng có thể làm chủ.

## Xem Thêm
- [Về PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)
