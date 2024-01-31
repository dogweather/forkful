---
title:                "Viết một tệp văn bản"
date:                  2024-01-28T22:13:20.981492-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"

category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì và Tại Sao?

Việc viết một tệp văn bản trong Python có nghĩa là lưu chuỗi vào một tệp trên ổ đĩa của bạn. Lập trình viên làm điều này để giữ dữ liệu qua các phiên, ghi nhật ký thông tin, hoặc xuất kết quả dễ đọc.

## Cách thức:

Viết vào tệp rất đơn giản. Sử dụng câu lệnh `with` để mở tệp, sau đó gọi `write()`.

```Python
# Viết vào tệp trong Python
with open('example.txt', 'w') as file:
    file.write("Xin chào, Thế giới!")

# Đọc tệp đã viết
with open('example.txt', 'r') as file:
    print(file.read())
```

Kết quả Mẫu:
```
Xin chào, Thế giới!
```

Thêm vào một tệp hiện có mà không ghi đè lên:

```Python
# Thêm vào tệp trong Python
with open('example.txt', 'a') as file:
    file.write("\nHẹn gặp lại, Thế giới!")

# Đọc tệp đã thêm
with open('example.txt', 'r') as file:
    print(file.read())
```

Kết quả Mẫu:
```
Xin chào, Thế giới!
Hẹn gặp lại, Thế giới!
```

## Sâu Hơn

Việc viết tệp văn bản có nguồn gốc từ các hệ thống máy tính sớm. Đây là hình thức cơ bản nhất của việc duy trì dữ liệu và trao đổi giữa các chương trình và hệ thống. Mặc dù có các lựa chọn khác như cơ sở dữ liệu cho dữ liệu phức tạp, tệp văn bản vẫn được sử dụng rộng rãi vì tính đơn giản và khả năng đọc của con người. Khi viết tệp, Python xử lý nhiều phức tạp về buffering và quản lý bộ nhớ, diễn ra ngầm và cung cấp các chế độ khác nhau (ví dụ, viết 'w', thêm vào 'a') cho các trường hợp sử dụng khác nhau.

## Xem Thêm

- Tài liệu chính thức của Python về nhập/xuất tệp: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Hướng dẫn của Real Python về nhập/xuất tệp: https://realpython.com/read-write-files-python/
- Đọc thêm về xử lý tệp trong Python với các trình quản lý ngữ cảnh: https://docs.python.org/3/reference/compound_stmts.html#the-with-statement
