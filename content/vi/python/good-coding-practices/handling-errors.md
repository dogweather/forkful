---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:42.394946-07:00
description: "L\xE0m th\u1EBF n\xE0o: K\u1EBFt qu\u1EA3 m\u1EABu khi nh\u1EADp m\u1ED9\
  t s\u1ED1 kh\xF4ng h\u1EE3p l\u1EC7 cho kh\u1ED1i \u0111\u1EA7u ti\xEAn."
lastmod: '2024-04-05T21:53:37.536902-06:00'
model: gpt-4-0125-preview
summary: "K\u1EBFt qu\u1EA3 m\u1EABu khi nh\u1EADp m\u1ED9t s\u1ED1 kh\xF4ng h\u1EE3\
  p l\u1EC7 cho kh\u1ED1i \u0111\u1EA7u ti\xEAn."
title: "X\u1EED l\xFD l\u1ED7i"
weight: 16
---

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
