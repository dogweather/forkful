---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:26.181054-07:00
description: "REPL, hay V\xF2ng L\u1EB7p \u0110\u1ECDc-\u0110\xE1nh Gi\xE1-In, l\xE0\
  \ m\u1ED9t m\xF4i tr\u01B0\u1EDDng l\u1EADp tr\xECnh t\u01B0\u01A1ng t\xE1c l\u1EA5\
  y \u0111\u1EA7u v\xE0o t\u1EEB ng\u01B0\u1EDDi d\xF9ng, th\u1EF1c thi ch\xFAng v\xE0\
  \ tr\u1EA3 v\u1EC1 k\u1EBFt qu\u1EA3. C\xE1c nh\xE0 ph\xE1t tri\u1EC3n\u2026"
lastmod: 2024-02-19 22:04:56.446939
model: gpt-4-0125-preview
summary: "REPL, hay V\xF2ng L\u1EB7p \u0110\u1ECDc-\u0110\xE1nh Gi\xE1-In, l\xE0 m\u1ED9\
  t m\xF4i tr\u01B0\u1EDDng l\u1EADp tr\xECnh t\u01B0\u01A1ng t\xE1c l\u1EA5y \u0111\
  \u1EA7u v\xE0o t\u1EEB ng\u01B0\u1EDDi d\xF9ng, th\u1EF1c thi ch\xFAng v\xE0 tr\u1EA3\
  \ v\u1EC1 k\u1EBFt qu\u1EA3. C\xE1c nh\xE0 ph\xE1t tri\u1EC3n\u2026"
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
---

{{< edit_this_page >}}

## Gì & Tại Sao?
REPL, hay Vòng Lặp Đọc-Đánh Giá-In, là một môi trường lập trình tương tác lấy đầu vào từ người dùng, thực thi chúng và trả về kết quả. Các nhà phát triển sử dụng nó để nhận phản hồi tức thì, gỡ lỗi và thử nghiệm các khái niệm mã hóa một cách nhanh chóng mà không cần phải biên dịch và chạy một chương trình đầy đủ.

## Cách thực hiện:
Trong Fish, shell tương tác là chế độ mặc định khi bạn khởi động nó. Dưới đây là một ví dụ về cách nó hoạt động:

```Fish Shell
> set màu xanh
> echo "Bầu trời là $màu"
Bầu trời là xanh
```

Bạn cũng có thể chạy các hàm đã được xây dựng sẵn và thử nghiệm với các thay thế lệnh:

```Fish Shell
> function hò reo
      echo "Hãy Fish $argv!"
  end
> hò reo Lập trình viên
Hãy Fish Lập trình viên!
```

Không chỉ định nghĩa các hàm, bạn cũng có thể thực thi các đoạn mã ngay lập tức và xem kết quả ngay lập tức:

```Fish Shell
> math "40 / 2"
20
```

## Sâu hơn
Khái niệm về REPL đã có từ những năm 1960 với ngôn ngữ lập trình Lisp. Hình thức lập trình tương tác này đã đặt ra tiêu chuẩn cho các môi trường như `ipython` của Python và `irb` của Ruby. Fish tiếp tục xu hướng này với tập trung vào tính thân thiện với người dùng và sử dụng tương tác.

Fish khác biệt với các shell khác như Bash ở chỗ nó được thiết kế với tính tương tác là ưu tiên ngay từ đầu. Nó cung cấp tính năng nổi bật cú pháp, gợi ý tự động và hoàn thành tab, làm cho nó mạnh mẽ khi sử dụng trong một quy trình làm việc kiểu REPL. Hơn nữa, các lệnh của bạn được nhớ và có thể tìm kiếm lại, làm cho việc thử nghiệm lặp đi lặp lại trở nên dễ dàng.

Các lựa chọn thay thế cho REPL của Fish có thể là `bash` hoặc `zsh` khi kết hợp với các tiện ích mở rộng như `bash-completion` hoặc `oh-my-zsh`, nhưng Fish có xu hướng cung cấp một trải nghiệm ngay lập tức phong phú hơn.

## Xem thêm:
- Tài liệu của Fish: https://fishshell.com/docs/current/index.html
- So sánh thú vị giữa Fish và các shell khác: https://www.slant.co/versus/2209/3686/~fish_vs_bash
- Sâu hơn về REPL: https://en.wikipedia.org/wiki/Read–eval–print_loop
- Lập trình tương tác trong Lisp, cái nhìn lịch sử: http://www.paulgraham.com/ilisp.html
