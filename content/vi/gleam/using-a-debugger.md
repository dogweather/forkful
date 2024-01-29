---
title:                "Sử dụng bộ gỡ lỗi"
date:                  2024-01-28T22:09:32.018625-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng bộ gỡ lỗi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?
Sử dụng một trình gỡ rối cơ bản là bạn đang trở thành thám tử trong mã lập trình của mình, điều tra tìm kiếm lỗi và tìm hiểu tại sao mọi thứ không chạy mượt mà. Lập trình viên làm điều này bởi vì, hãy đối diện với nó, lỗi là điều không thể tránh khỏi, và diệt trừ chúng một cách hiệu quả nghĩa là làm cho mã của bạn hoạt động nhanh chóng, đáng tin cậy hơn.

## Làm thế nào:
Gleam hiện tập trung vào hệ sinh thái Erlang để lấy công cụ, vì vậy bạn sẽ thường sử dụng các công cụ như `rebar3`, `observer` và `debugger` để gỡ rối. Dưới đây là cách để bắt tay vào việc gỡ rối:

```gleam
// Trong cấu hình rebar của bạn, đảm bảo bạn có những dòng này để bao gồm thông tin debug:
{erl_opts, [debug_info]}.

// Chạy một shell Erlang với ứng dụng của bạn được tải
rebar3 shell

// Bên trong shell, bạn có thể khởi động trình gỡ rối
1> debugger:start().
```

Đơn giản, phải không? Giao diện người dùng `debugger` hiện lên, và bạn có thể thiết lập các điểm dừng, bước qua mã, và theo dõi biến tuỳ ý. Bạn sẽ không thấy mã Gleam trực tiếp, nhưng bạn sẽ thấy mã Erlang mà nó biên dịch thành, điều này vẫn khá hữu ích.

## Sâu Hơn
Gleam là một ngôn ngữ mới, vì vậy mặc dù nó đứng trên vai của hệ sinh thái Erlang, các công cụ gỡ rối Gleam gốc chưa được đưa ra. Điều đó có nghĩa là chúng ta đang sử dụng các công cụ đã được thử và đúc kết của Erlang, và đó không phải là điều tồi. Trình gỡ rối của Erlang đã xuất hiện từ những năm '90, được mài giũa qua nhiều năm diệt trừ các lỗi khó chịu trong các hệ thống nơi độ tin cậy là chìa khóa.

Đối với các phương pháp thay thế, truy vết là một phương pháp mạnh mẽ trong thế giới BEAM (đó là máy ảo chạy mã Erlang và Elixir). Sử dụng `rebar3` bạn có thể tận dụng các công cụ như `recon` để truy vết các lời gọi hàm và đào sâu vào các vấn đề về hiệu suất.

Việc chuyển đổi giữa việc viết Gleam và gỡ rối trong Erlang có thể cảm thấy như bạn đang dịch suy nghĩ của mình trên tức thì. Nhưng lợi ích là bạn có cái nhìn vào thế giới Erlang, hiểu các khối xây dựng của ứng dụng của mình trong hình thức thời gian chạy.

## Xem thêm
Để mở rộng bộ công cụ gỡ rối của bạn, hãy kiểm tra:

- Tài liệu của trình gỡ rối Erlang: [https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)
- Thư viện `recon` cho Erlang: [https://ferd.github.io/recon/](https://ferd.github.io/recon/)
