---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:26.181054-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Fish, shell t\u01B0\u01A1ng t\xE1\
  c l\xE0 ch\u1EBF \u0111\u1ED9 m\u1EB7c \u0111\u1ECBnh khi b\u1EA1n kh\u1EDFi \u0111\
  \u1ED9ng n\xF3. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 v\u1EC1\
  \ c\xE1ch n\xF3 ho\u1EA1t \u0111\u1ED9ng."
lastmod: '2024-03-13T22:44:37.212837-06:00'
model: gpt-4-0125-preview
summary: "Trong Fish, shell t\u01B0\u01A1ng t\xE1c l\xE0 ch\u1EBF \u0111\u1ED9 m\u1EB7\
  c \u0111\u1ECBnh khi b\u1EA1n kh\u1EDFi \u0111\u1ED9ng n\xF3."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

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
