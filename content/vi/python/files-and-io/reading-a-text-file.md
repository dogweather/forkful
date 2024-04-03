---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:14.586805-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: ."
lastmod: '2024-03-13T22:44:36.120449-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0110\u1ECDc m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 22
---

## Cách thực hiện:
```python
# Đọc toàn bộ tệp một lần
with open('example.txt', 'r') as file:
    content = file.read()
    print(content)

# Đọc từng dòng một
with open('example.txt', 'r') as file:
    for line in file:
        print(line.strip())
```

Kết quả mẫu:
```
Đây là dòng đầu tiên của tệp.
Và đây là dòng thứ hai.
```

## Sâu hơn
Việc đọc tệp văn bản là cơ bản - và đã tồn tại từ những ngày đầu của lập trình. Hàm `open` đơn giản của Python có nguồn gốc từ hàm `fopen` của thư viện chuẩn C. Một số phương thức thay thế cho việc đọc tệp văn bản bao gồm việc sử dụng các thư viện như `pandas` cho CSV hoặc `json` cho các tệp JSON. Nội bộ, khi bạn đọc một tệp, Python yêu cầu hệ điều hành mở một luồng tệp, giống như một băng tải chuyển dữ liệu từ tệp sang chương trình của bạn.

Đối với các tệp lớn, thay vì sử dụng `read()` để tải mọi thứ vào bộ nhớ, hãy sử dụng `readline()` hoặc lặp qua đối tượng tệp với vòng lặp `for` để xử lý từng dòng một cách hiệu quả và tiết kiệm bộ nhớ. Trong khi `with open` là cách tiếp cận hiện đại tự động đóng tệp, các kịch bản cũ có thể sử dụng `file.close()` để làm điều này một cách thủ công, mặc dù nó dễ gặp lỗi nếu ngoại lệ xảy ra trước khi gọi đóng.

## Xem thêm
- Tài liệu Python về IO: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Hướng dẫn Python về Tệp của Real Python: https://realpython.com/read-write-files-python/
- Tài liệu chính thức của Python cho `open`: https://docs.python.org/3/library/functions.html#open
