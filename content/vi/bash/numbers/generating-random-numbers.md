---
title:                "Sinh số ngẫu nhiên"
date:                  2024-01-28T22:01:10.974573-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sinh số ngẫu nhiên"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Việc tạo số ngẫu nhiên trong Bash mang lại cách thức để bổ sung tính không dự đoán trước vào các script, điều này rất cần thiết cho các tác vụ như tạo mật khẩu an toàn, mô phỏng dữ liệu, hoặc cho lập trình game. Các lập trình viên tận dụng khả năng này để thêm biến thể vào script của họ hoặc để kiểm thử chương trình của họ dưới nhiều điều kiện được tạo ra một cách ngẫu nhiên.

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
