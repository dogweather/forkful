---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:10.974573-07:00
description: "Vi\u1EC7c t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn trong Bash mang l\u1EA1\
  i c\xE1ch th\u1EE9c \u0111\u1EC3 b\u1ED5 sung t\xEDnh kh\xF4ng d\u1EF1 \u0111o\xE1\
  n tr\u01B0\u1EDBc v\xE0o c\xE1c script, \u0111i\u1EC1u n\xE0y r\u1EA5t c\u1EA7n\
  \ thi\u1EBFt cho c\xE1c t\xE1c v\u1EE5 nh\u01B0 t\u1EA1o m\u1EADt\u2026"
lastmod: '2024-03-13T22:44:36.870898-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o s\u1ED1 ng\u1EABu nhi\xEAn trong Bash mang l\u1EA1i c\xE1\
  ch th\u1EE9c \u0111\u1EC3 b\u1ED5 sung t\xEDnh kh\xF4ng d\u1EF1 \u0111o\xE1n tr\u01B0\
  \u1EDBc v\xE0o c\xE1c script, \u0111i\u1EC1u n\xE0y r\u1EA5t c\u1EA7n thi\u1EBF\
  t cho c\xE1c t\xE1c v\u1EE5 nh\u01B0 t\u1EA1o m\u1EADt kh\u1EA9u an to\xE0n, m\xF4\
  \ ph\u1ECFng d\u1EEF li\u1EC7u, ho\u1EB7c cho l\u1EADp tr\xECnh game."
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
weight: 12
---

## Cách thực hiện:
Trong Bash, biến `$RANDOM` là lựa chọn hàng đầu để tạo số ngẫu nhiên. Mỗi lần bạn tham chiếu đến nó, Bash cung cấp một số nguyên giả ngẫu nhiên từ 0 đến 32767. Hãy khám phá một số ví dụ thực tế:

```Bash
# Sử dụng cơ bản của $RANDOM
echo $RANDOM

# Tạo một số ngẫu nhiên trong phạm vi chỉ định (ở đây là 0-99)
echo $(( RANDOM % 100 ))

# Tạo một số ngẫu nhiên "an toàn" hơn, phù hợp cho mật khẩu hoặc khóa
# Sử dụng /dev/urandom với lệnh od
head -c 8 /dev/urandom | od -An -tu4

# Gieo số cho RANDOM để tái tạo được
RANDOM=42; echo $RANDOM
```

Kết quả mẫu (chú ý: kết quả thực tế có thể thay đổi vì các số là ngẫu nhiên):
```Bash
16253
83
3581760565
17220
```

## Sâu hơn
Cơ chế đằng sau `$RANDOM` của Bash tạo ra số giả ngẫu nhiên, nghĩa là chúng theo một thuật toán và, lý thuyết, có thể dự đoán trước - một khuyết điểm về bảo mật cho các ứng dụng cần tính không dự đoán trước thực sự. Các ứng dụng mật mã hiện đại thường cần ngẫu nhiên được rút ra từ hiện tượng vật lý hoặc từ phần cứng được thiết kế đặc biệt để tạo dữ liệu ngẫu nhiên, như `/dev/urandom` hoặc `/dev/random` trong Linux, thu thập tiếng ồn môi trường.

Đối với các tác vụ không quan trọng về bảo mật hoặc bình thường, `$RANDOM` đủ và mang lại lợi ích về sự đơn giản. Tuy nhiên, đối với mục đích mật mã hoặc khi chất lượng ngẫu nhiên là quan trọng, các nhà phát triển nên tìm đến những công cụ và ngôn ngữ khác được thiết kế với mật mã trong đầu, như OpenSSL hoặc ngôn ngữ lập trình có thư viện tạo số ngẫu nhiên mạnh mẽ.

Dù `$RANDOM` của Bash phục vụ mục đích trong các script cần số ngẫu nhiên cơ bản, nhưng hạn chế của nó nên thúc đẩy các nhà phát triển hướng đến giải pháp mạnh mẽ hơn cho các ứng dụng nơi chất lượng hoặc bảo mật của tính ngẫu nhiên là quan trọng.
