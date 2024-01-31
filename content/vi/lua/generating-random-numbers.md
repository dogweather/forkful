---
title:                "Sinh số ngẫu nhiên"
date:                  2024-01-28T22:01:07.088760-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sinh số ngẫu nhiên"

category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/lua/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tạo số ngẫu nhiên trong lập trình là về việc sản xuất các giá trị số không thể đoán trước được có thể sử dụng cho một loạt các mục đích như mô phỏng, trò chơi hoặc ứng dụng bảo mật. Các lập trình viên sử dụng tính năng này để giới thiệu một yếu tố không chắc chắn hoặc mô phỏng sự biến động của đời sống thực trong các dự án của họ.

## Làm thế nào:

Lua cung cấp hỗ trợ sẵn có cho việc tạo số ngẫu nhiên thông qua hàm `math.random`. Hàm này có thể được sử dụng theo nhiều cách, tùy thuộc vào kết quả mong muốn:

1. **Tạo một số thực ngẫu nhiên giữa 0 và 1:**

```Lua
print(math.random())
```

Kết quả mẫu có thể là `0.13117647051304`. Mỗi lần chạy sẽ sinh ra một giá trị khác nhau.

2. **Tạo một số nguyên ngẫu nhiên trong một phạm vi xác định:**

Để tạo một số nguyên ngẫu nhiên giữa hai giới hạn, bao gồm cả hai, bạn cần phải thiết lập hạt giống sử dụng `math.randomseed(os.time())` để đạt được sự biến đổi, sau đó gọi `math.random` với hai đối số:

```Lua
math.randomseed(os.time())
print(math.random(1, 10)) -- Sinh một số nguyên ngẫu nhiên giữa 1 và 10
```

Kết quả mẫu có thể là `7`. Một lần nữa, kết quả sẽ thay đổi với mỗi lần thực thi.

Việc thiết lập hạt giống với `math.randomseed` là cực kỳ quan trọng vì nếu không, `math.random` có thể sinh ra cùng một chuỗi số mỗi khi một chương trình được chạy. Thông thường, việc gieo hạt với thời gian hiện tại, `os.time()`, đảm bảo các chuỗi khác nhau cho mỗi lần thực thi.

## Sâu hơn

Cơ chế đằng sau việc tạo số ngẫu nhiên trong Lua (và hầu hết các ngôn ngữ lập trình) không thực sự là ngẫu nhiên mà là giả ngẫu nhiên, được sinh ra bởi một thuật toán. Các trình sinh số giả ngẫu nhiên (PRNGs) là có tính xác định và yêu cầu một giá trị hạt giống để bắt đầu chuỗi sinh số. Việc chọn lựa hạt giống là cực kỳ quan trọng cho chất lượng ngẫu nhiên, đó là lý do tại sao sử dụng thời gian hiện tại là một thực hành phổ biến.

Về lịch sử, khả năng tạo số ngẫu nhiên của Lua đã phát triển. Các phiên bản trước dựa vào hàm `rand()` của thư viện tiêu chuẩn C, có chất lượng và hiệu suất thay đổi giữa các triển khai. Phiên bản hiện tại của Lua cải thiện điều này bằng cách có thể sử dụng các cơ chế mạnh mẽ hơn tùy thuộc vào nền tảng cơ sở, cung cấp sự nhất quán và tiện ích cao hơn trong việc tạo số ngẫu nhiên.

Đối với các dự án yêu cầu sự ngẫu nhiên ở cấp độ mật mã, chức năng Lua tích hợp có thể không đủ do bản chất xác định của PRNGs. Trong những trường hợp như vậy, các lập trình viên thường chuyển sang sử dụng thư viện bên ngoài hoặc API cụ thể của hệ thống có thể cung cấp số ngẫu nhiên không xác định phù hợp cho các ứng dụng an ninh cao.
