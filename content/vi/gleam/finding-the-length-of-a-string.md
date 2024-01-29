---
title:                "Tìm chiều dài của một chuỗi ký tự"
date:                  2024-01-28T22:00:48.679218-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm chiều dài của một chuỗi ký tự"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Điều Gì & Tại Sao?

Tìm độ dài của một chuỗi nghĩa là đếm số ký tự mà nó chứa. Lập trình viên thực hiện điều này để xác nhận đầu vào, cắt chuỗi, hoặc chỉ đơn giản là muốn biết đoạn văn bản đó dày cỡ nào.

## Làm Thế Nào:

Trong Gleam, việc tìm ra độ dài của một chuỗi là một câu lệnh một dòng. Đây là cách bạn thực hiện:

```Gleam
import gleam/io
import gleam/string

fn main() {
  let my_string = "Gleam shines!"
  let length = string.len(my_string)
  io.println(length) // Đầu ra: 12
}
```

Chỉ cần sử dụng hàm `string.len` và truyền vào chuỗi của bạn. Bùm! Bạn nhận được độ dài.

## Sâu Hơn

Ngày xưa, các chuỗi giống như những lời mê thuật—khó xử lý, mỗi ngôn ngữ đều có cách thần chú riêng của mình. Trong các ngôn ngữ như C, bạn phải điều hướng thủ công qua một mảng ký tự cho đến khi gặp ký tự kết thúc null (`'\0'`) để tìm độ dài của một chuỗi. Đau đớn, phải không?

Tuy nhiên, Gleam giữ mọi thứ đơn giản. Nó chạy trên BEAM VM—nơi chứa Erlang và Elixir—nơi mà chuỗi được xem xét như một chuỗi của các byte. Đúng vậy, các byte, không phải các ký tự. Điều này là điểm chính vì trong Unicode, các ký tự có thể hơn một byte. Các chuỗi của Gleam được mã hóa UTF-8, vì vậy một ký tự đơn lẻ có thể từ 1 đến 4 byte.

Điều này cần lưu ý—`string.len` cho bạn biết số lượng byte, không phải số lượng cụm ký tự Unicode (những gì chúng ta thường nghĩ về như là các ký tự). Vì vậy, đối với các chuỗi ASCII (nơi mỗi ký tự là một byte duy nhất), độ dài bằng byte bằng số lượng ký tự. Đối với các chuỗi chứa emoji hoặc các ký tự nhiều byte khác, không hẳn như vậy.

Đối với một giải pháp nhanh chóng, hiện tại Gleam không có sẵn giải pháp nào trong thư viện. Bạn sẽ cần phải kéo một thư viện khác vào hoặc tự viết một số mã nếu bạn cần đếm cụm grapheme.

## Xem Thêm

Hãy tìm hiểu sâu hơn về cách xử lý chuỗi trong Gleam trong tài liệu chính thức:


Và đối với biểu đồ cụm grapheme, byte và ký tự, kiểm tra tại:

- Trình xem Cụm Grapheme Unicode: [https://util.unicode.org/UnicodeJsps/list-unicodeset.jsp?a=%5B%3AGrapheme_Cluster_Break%3DControl%3A%5D&abb=on&esc=on&g=&i=](https://util.unicode.org/UnicodeJsps/list-unicodeset.jsp?a=%5B%3AGrapheme_Cluster_Break%3DControl%3A%5D&abb=on&esc=on&g=&i=)
