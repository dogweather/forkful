---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:42.572498-07:00
description: "Vi\u1EC7c thao t\xE1c v\u1EDBi c\xE1c t\u1EC7p tin s\u1EED d\u1EE5ng\
  \ CLI (Command Line Interface) b\u1EB1ng c\xE1ch nh\u1EADp l\u1EC7nh \u0111\u01A1\
  n v\u1EDBi Bash bao g\u1ED3m vi\u1EC7c d\xF9ng c\xE1c script Bash hay l\u1EC7nh\
  \ \u0111\u1EC3 th\u1EF1c\u2026"
lastmod: '2024-02-25T18:49:35.217287-07:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c thao t\xE1c v\u1EDBi c\xE1c t\u1EC7p tin s\u1EED d\u1EE5ng CLI\
  \ (Command Line Interface) b\u1EB1ng c\xE1ch nh\u1EADp l\u1EC7nh \u0111\u01A1n v\u1EDB\
  i Bash bao g\u1ED3m vi\u1EC7c d\xF9ng c\xE1c script Bash hay l\u1EC7nh \u0111\u1EC3\
  \ th\u1EF1c\u2026"
title: "Thao t\xE1c v\u1EDBi c\xE1c t\u1EC7p tin b\u1EB1ng c\xE1c l\u1EC7nh CLI ch\u1EC9\
  \ m\u1ED9t d\xF2ng"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Việc thao tác với các tệp tin sử dụng CLI (Command Line Interface) bằng cách nhập lệnh đơn với Bash bao gồm việc dùng các script Bash hay lệnh để thực hiện các thao tác trên tệp tin, như tạo, đọc, cập nhật, hoặc xóa chúng, tất cả từ terminal. Lập trình viên thực hiện điều này vì tính hiệu quả, tự động hóa, và bởi vì nó đặc biệt mạnh mẽ trong việc xử lý các thao tác tệp tin trên máy chủ Linux hoặc hệ thống, nơi mà giao diện đồ họa có thể không khả dụng.

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
