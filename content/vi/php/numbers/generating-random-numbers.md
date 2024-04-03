---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:47.207669-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: PHP cung c\u1EA5p m\u1ED9t s\u1ED1 h\xE0\
  m \u0111\u1EC3 sinh s\u1ED1 ng\u1EABu nhi\xEAn, nh\u01B0ng c\xE1c h\xE0m th\u01B0\
  \u1EDDng \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng nh\u1EA5t l\xE0 `rand()`, `mt_rand()`,\
  \ v\xE0 cho m\u1EE5c \u0111\xEDch m\u1EADt m\xE3\u2026"
lastmod: '2024-03-13T22:44:36.760777-06:00'
model: gpt-4-0125-preview
summary: "PHP cung c\u1EA5p m\u1ED9t s\u1ED1 h\xE0m \u0111\u1EC3 sinh s\u1ED1 ng\u1EAB\
  u nhi\xEAn, nh\u01B0ng c\xE1c h\xE0m th\u01B0\u1EDDng \u0111\u01B0\u1EE3c s\u1EED\
  \ d\u1EE5ng nh\u1EA5t l\xE0 `rand()`, `mt_rand()`, v\xE0 cho m\u1EE5c \u0111\xED\
  ch m\u1EADt m\xE3 h\xF3a, `random_int()`."
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

## Cách thực hiện:
PHP cung cấp một số hàm để sinh số ngẫu nhiên, nhưng các hàm thường được sử dụng nhất là `rand()`, `mt_rand()`, và cho mục đích mật mã hóa, `random_int()`.

Để tạo một số ngẫu nhiên đơn giản giữa 0 và getrandmax() (giá trị lớn nhất có thể trả về bởi `rand()`), bạn có thể sử dụng:

```PHP
echo rand();
```

Đối với một dải giá trị cụ thể hơn, như giữa 1 và 100:

```PHP
echo rand(1, 100);
```

Tuy nhiên, `mt_rand()` là một lựa chọn tốt hơn về tốc độ và tính ngẫu nhiên:

```PHP
echo mt_rand(1, 100);
```

Kết quả cho cả hai có thể là bất kỳ giá trị nào giữa 1 và 100, tùy thuộc vào việc ngẫu nhiên hóa, ví dụ, `42`.

Đối với ngữ cảnh mật mã hóa hay bảo mật, nơi mà sự không thể đoán trước là rất quan trọng, `random_int()` là lựa chọn được ưa chuộng vì nó sinh số nguyên giả ngẫu nhiên mật mã hóa an toàn:

```PHP
echo random_int(1, 100);
```

Một lần nữa, kết quả là một số ngẫu nhiên giữa 1 và 100, như `84`, nhưng với một bảo đảm mạnh mẽ hơn về tính ngẫu nhiên.

## Sâu hơn
Hàm `rand()` đã có mặt trong PHP từ những phiên bản đầu tiên, phục vụ như là cách tiếp cận ban đầu để sinh số ngẫu nhiên. Tuy nhiên, nó không phải là lựa chọn tốt nhất cho các ứng dụng cần một mức độ ngẫu nhiên cao do thuật toán tương đối dễ dự đoán của nó.

`mt_rand()`, được giới thiệu trong PHP 4, dựa trên thuật toán Mersenne Twister - vượt trội về tốc độ và mức độ ngẫu nhiên có thể tạo ra so với `rand()`. Nó nhanh chóng trở thành lựa chọn ưa thích cho hầu hết nhu cầu không mật mã.

Đối với các ứng dụng nhạy cảm với bảo mật, `random_int()` được giới thiệu trong PHP 7 để sinh số nguyên giả ngẫu nhiên mật mã hóa an toàn sử dụng các byte ngẫu nhiên từ bộ sinh số ngẫu nhiên của hệ thống. Nó an toàn đáng kể hơn so với `rand()` hoặc `mt_rand()`, làm cho nó trở thành lựa chọn tốt nhất để sinh token, khóa, hoặc các yếu tố khác nơi tính dự đoán có thể dẫn đến lỗ hổng bảo mật.

Mặc dù có những cải tiến này, điều quan trọng là phải chọn đúng hàm dựa trên ngữ cảnh ứng dụng. Đối với việc sử dụng chung, `mt_rand()` là đủ, nhưng đối với bất cứ điều gì có thể được nhắm mục tiêu hoặc bị khai thác, `random_int()` là con đường phải đi, cung cấp cả tính ngẫu nhiên và bảo mật.
