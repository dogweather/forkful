---
title:                "Xử lý lỗi"
aliases:
- /vi/python/handling-errors.md
date:                  2024-01-28T22:02:42.394946-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xử lý lỗi"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Xử lý lỗi trong Python (hoặc bất kỳ ngôn ngữ lập trình nào) là việc dự đoán những điều không mong đợi - đó là nghệ thuật quản lý một cách nhẹ nhàng khi mọi thứ diễn biến không như mong muốn trong code của bạn. Chúng ta làm điều này để ngăn chặn sự cố, hướng dẫn người dùng, và làm cho chương trình của mình vững chắc và đáng tin cậy.

## Làm thế nào:

``` Python
# Khối try-except cơ bản
try:
    # code có rủi ro
    number = int(input("Nhập một số: "))
except ValueError:
    # xử lý lỗi
    print("Đó không phải là một số!")

# Chỉ định nhiều ngoại lệ
try:
    # code có thể gây ra các ngoại lệ khác nhau
    result = 10 / int(input("Nhập một số chia: "))
except ZeroDivisionError:
    print("Ôi! Không thể chia cho số không.")
except ValueError:
    print("Mình cần một số, bạn ơi.")

# Sử dụng else và finally
try:
    number = int(input("Nhập một số để bình phương: "))
except ValueError:
    print("Mình đã nói là một số!")
else:
    # không có lỗi nào xảy ra
    print("Số của bạn khi bình phương là:", number**2)
finally:
    # luôn luôn thực thi
    print("Cảm ơn bạn đã thử nghiệm!")
```

Kết quả mẫu khi nhập một số không hợp lệ cho khối đầu tiên:
```
Nhập một số: hello
Đó không phải là một số!
```

## Tìm hiểu sâu hơn

Từ thời điểm bình minh của lập trình, xử lý lỗi đã là một phần quan trọng. Cách tiếp cận ban đầu còn sơ khai, như kiểm tra điều kiện trước mỗi hoạt động có rủi ro. Cú pháp `try-except` của Python đã được phát triển dựa trên di sản của việc xử lý ngoại lệ trong các ngôn ngữ cũ hơn như C++ và Java, đơn giản hóa quá trình này.

Khi bạn `try` một khối code, Python sẽ theo dõi bất kỳ ngoại lệ nào. Nếu một lỗi xuất hiện, khối `except` sẽ bắt lấy nó. Bạn có thể cụ thể về những ngoại lệ mà bạn bắt, hoặc bắt hết tất cả với một `except` trống. Tuy nhiên, cụ thể từng cái là cách tiếp cận tốt hơn – nó chính xác, không phải là lưới bắt tất cả.

`else` và `finally` là những phần phụ trong khái niệm này. Khối `else` sẽ chạy nếu khối try không có lỗi. `finally` là người bạn đáng tin cậy luôn chạy bất kể chuyện gì xảy ra - hãy nghĩ về các hoạt động dọn dẹp.

Có sự thay thế? Chắc chắn là có. Một số ngôn ngữ sử dụng mã trả về thay vì ngoại lệ. Bạn cũng có thể gặp các câu lệnh `with` dùng để xử lý tài nguyên hoặc `assertions` kiểm tra điều kiện trong quá trình phát triển. Nhưng khi chúng ta nói về chiến lược xử lý lỗi vững chắc, mô hình try-catch nổi bật với độ rõ ràng và cấu trúc của nó.

## Xem thêm

Dưới đây là một số tài nguyên bổ sung tốt để đào sâu thêm:

- Tài liệu chính thức của Python về lỗi và ngoại lệ: [Python Docs – Errors and Exceptions](https://docs.python.org/3/tutorial/errors.html)
- Hướng dẫn của Real Python về chủ đề này: [Real Python - Khối try/except/else/finally](https://realpython.com/python-exceptions/)
- Một cuộc thảo luận sâu sắc về các phương pháp xử lý lỗi tốt nhất: [Stack Overflow – Làm thế nào để tôi bỏ qua ngoại lệ một cách đúng đắn?](https://stackoverflow.com/questions/4990718/about-catching-any-exception)
