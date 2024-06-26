---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:42.572498-07:00
description: "L\xE0m th\u1EBF n\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t s\u1ED1\
  \ l\u1EC7nh \u0111\u01A1n \u0111\u1EA7y m\u1EA1nh m\u1EBD v\xE0 nh\u1EEFng g\xEC\
  \ ch\xFAng c\xF3 th\u1EC3 th\u1EF1c hi\u1EC7n: 1. **T\u1EA1o m\u1ED9t t\u1EC7p tin\
  \ v\xE0 vi\u1EBFt v\u0103n b\u1EA3n v\xE0o \u0111\xF3:**."
lastmod: '2024-03-13T22:44:36.872190-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t s\u1ED1 l\u1EC7nh \u0111\u01A1\
  n \u0111\u1EA7y m\u1EA1nh m\u1EBD v\xE0 nh\u1EEFng g\xEC ch\xFAng c\xF3 th\u1EC3\
  \ th\u1EF1c hi\u1EC7n."
title: "Thao t\xE1c v\u1EDBi c\xE1c t\u1EC7p tin b\u1EB1ng c\xE1c l\u1EC7nh CLI ch\u1EC9\
  \ m\u1ED9t d\xF2ng"
weight: 31
---

## Làm thế nào:
Dưới đây là một số lệnh đơn đầy mạnh mẽ và những gì chúng có thể thực hiện:

1. **Tạo một tệp tin và viết văn bản vào đó:**
```Bash
echo "Hello, Linux Journal Readers!" > greetings.txt
```
Điều này tạo ra (hoặc ghi đè nếu đã tồn tại) tệp `greetings.txt` với cụm từ "Hello, Linux Journal Readers!".

2. **Thêm văn bản vào một tệp tin đã tồn tại:** 
```Bash
echo "Welcome to Bash programming." >> greetings.txt
```
Điều này thêm một dòng mới "Welcome to Bash programming." vào cuối tệp `greetings.txt`.

3. **Đọc nội dung của một tệp tin:**
```Bash
cat greetings.txt
```
Kết quả:
```
Hello, Linux Journal Readers!
Welcome to Bash programming.
```

4. **Tìm kiếm một dòng cụ thể trong tệp (sử dụng `grep`):**
```Bash
grep "Bash" greetings.txt
```
Tìm và hiển thị các dòng chứa từ "Bash"; trong ví dụ này, nó trả về "Welcome to Bash programming."

5. **Liệt kê tất cả các tệp tin trong thư mục hiện tại được sắp xếp theo ngày sửa đổi:**
```Bash
ls -lt
```
Hiển thị các tệp được sắp xếp theo thời gian sửa đổi, mới nhất lên đầu.

6. **Đổi tên hàng loạt tệp `.txt` thành `.md` (Markdown):**
```Bash
for file in *.txt; do mv "$file" "${file%.txt}.md"; done
```
Vòng lặp này đi qua từng tệp `.txt` trong thư mục hiện tại và đổi tên chúng thành `.md`.

Những lệnh đơn CLI này tận dụng sức mạnh của Bash cho việc thao tác tệp tin nhanh chóng và hiệu quả, một kỹ năng mà bất kỳ lập trình viên nào cũng sẽ thấy không thể thiếu.

## Sâu hơn:
Vỏ Bash, một phần không thể thiếu trên hầu hết các hệ thống tương tự UNIX, đã phát triển từ Bourne Shell (sh), được giới thiệu trong Phiên bản 7 Unix vào năm 1979. Bash mở rộng khả năng của người tiền nhiệm với các tính năng viết kịch bản cải thiện đã khiến nó trở nên phổ biến đối với cả quản trị viên hệ thống và lập trình viên.

Mặc dù Bash rất mạnh mẽ trong việc thao tác tệp tin, nó cũng có những hạn chế của mình. Bởi vì dựa trên văn bản, các thao tác phức tạp (như những thao tác liên quan đến dữ liệu nhị phân) có thể trở nên cồng kềnh hoặc không hiệu quả so với việc sử dụng một ngôn ngữ lập trình được thiết kế với các khả năng đó trong tâm trí, như Python.

Các lựa chọn thay thế cho việc viết kịch bản Bash để thao tác tệp có thể bao gồm việc viết kịch bản Python sử dụng các thư viện `os` và `shutil`, có thể cung cấp cú pháp dễ đọc hơn và xử lý các kịch bản phức tạp một cách duyên dáng hơn. Tuy nhiên, sự phổ biến rộng rãi của Bash và hiệu quả của nó đối với đa số nhiệm vụ tệp tin đảm bảo sự phổ biến liên tục của nó.

Hơn nữa, việc hiểu rõ bên trong cách Bash xử lý tệp tin (mọi thứ đều là tệp trong triết lý Unix/Linux) và các lệnh được tích hợp sẵn (như `awk`, `sed`, `grep`, v.v.) có thể trao quyền cho lập trình viên để viết các kịch bản hiệu quả và hiệu lực hơn. Sự hiểu biết sâu sắc về các khả năng của shell cùng với bối cảnh lịch sử làm giàu khả năng thao tác tệp tin và thực hiện một loạt các nhiệm vụ trực tiếp từ dòng lệnh của lập trình viên.
