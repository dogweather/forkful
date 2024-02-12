---
title:                "Bắt đầu một dự án mới"
aliases:
- vi/powershell/starting-a-new-project.md
date:                  2024-01-28T22:08:56.583998-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bắt đầu một dự án mới"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/powershell/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc bắt đầu một dự án mới là việc đặt nền móng cho tác phẩm lập trình của bạn. Là những lập trình viên, chúng ta làm điều này để khởi đầu một ý tưởng mới hoặc triển khai các giải pháp một cách có tổ chức, dễ mở rộng.

## Cách thực hiện:
PowerShell làm cho việc tạo một dự án mới trở nên đơn giản. Bạn có thể muốn tạo một thư mục cho dự án của mình và thiết lập một kho lưu trữ git. Dưới đây là cách làm:

```PowerShell
# Tạo một thư mục mới cho dự án của bạn
New-Item -Path 'C:\MyProjects\NewCoolApp' -ItemType Directory

# Di chuyển đến thư mục mới của bạn
Set-Location -Path 'C:\MyProjects\NewCoolApp'

# Khởi tạo một kho lưu trữ git mới nếu bạn sử dụng kiểm soát phiên bản
git init
```

Kết quả mẫu:
```
    Directory: C:\MyProjects

Mode                 LastWriteTime         Length Name
----                 -------------         ------ ----
d-----          1/1/2023   12:00 AM                NewCoolApp
Initialized empty Git repository in C:/MyProjects/NewCoolApp/.git/
```

## Tìm hiểu sâu
PowerShell đã trở thành ngôn ngữ kịch bản được ưa chuộng cho tự động hóa Windows kể từ khi ra mắt vào năm 2006. Tạo dự án mới với PowerShell không chỉ là tạo thư mục; nó là một nghi lễ để thiết lập phạm vi dự án, định nghĩa các kịch bản, hoặc chuẩn bị các tác vụ tự động.

Mặc dù PowerShell được ưa chuộng trong thế giới Windows, người dùng Unix-like thường dựa vào 'bash' hoặc 'zsh' cho các tác vụ tương tự. Tuy nhiên, với sự ra đời của PowerShell Core, PowerShell đã bước vào vòng đấu đa nền tảng, cho phép lập kịch bản và tự động hóa trên nhiều hệ điều hành.

Sâu trong thiết kế của PowerShell là tính hướng đối tượng của nó, sử dụng cmdlets (phát âm là command-lets) xuất ra các đối tượng. Cmdlets như `New-Item` không chỉ tạo tệp hay thư mục; chúng tạo ra các đối tượng mà kịch bản của bạn có thể tương tác. Một cài đặt dự án mới có thể bao gồm việc thiết lập cấu trúc thư mục, tạo tệp README, thiết lập tệp .gitignore, hoặc thậm chí mẫu ra các tệp mã ban đầu.

Thực hiện một chu trình cài đặt dự án trong PowerShell có thể tận dụng nhiều cmdlets, từ thao tác tệp (`New-Item`) đến cấu hình môi trường (`Set-Location`). Kết hợp những thứ này với khả năng lập kịch bản của PowerShell có thể tạo ra các kịch bản cài đặt mạnh mẽ hành động như một người khởi xướng dự án, dựng nên cốt truyện của dự án với ít phiền hà nhất.

## Xem thêm
- [Lập kịch bản với PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [Sách Pro Git](https://git-scm.com/book/en/v2)
- [Hello World của GitHub](https://guides.github.com/activities/hello-world/)
- [PowerShell Core trên GitHub](https://github.com/PowerShell/PowerShell)
