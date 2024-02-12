---
title:                "Sử dụng vỏ tương tác (REPL)"
aliases: - /vi/fish-shell/using-an-interactive-shell-repl.md
date:                  2024-01-28T22:09:26.181054-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
