---
title:                "Trích xuất chuỗi con"
date:                  2024-01-28T21:59:48.115080-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trích xuất chuỗi con"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc trích xuất các chuỗi con giống như việc hái một quả anh đào từ bánh; bạn chọn một mảnh cụ thể từ một chuỗi. Các lập trình viên làm điều này để cô lập, phân tích hoặc thao tác với các khối dữ liệu nhỏ hơn từ các chuỗi văn bản lớn hơn.

## Làm thế nào:
Gleam có một số cách để xử lý việc trích xuất chuỗi con. Dưới đây là một số ví dụ:

```gleam
import gleam/string

let story = "Learning Gleam is fun!"

// Rút ra "Gleam" từ chuỗi
let gleam = string.slice(story, 9, 14)
assert gleam == Ok("Gleam")

// Bắt "fun!" sử dụng chỉ số âm
let fun = string.slice(story, -4, -1)
assert fun == Ok("fun!")

// Nếu các chỉ số nằm ngoài giới hạn, chúng ta sẽ nhận được lỗi
let oops = string.slice(story, 30, 40)
assert oops == Error(Nil)
```
Hàm `slice` là lựa chọn hàng đầu để lấy các chuỗi con. Các chỉ số được cung cấp là bao gồm ở đầu và loại trừ ở cuối. Các chỉ số âm đếm từ cuối.

## Sâu hơn
Việc trích xuất chuỗi con không phải là mới; nó đã cũ như núi trong lập trình. Ngày xưa, ngôn ngữ như C khiến bạn phải vượt qua nhiều rào cản với các con trỏ để bắt các chuỗi con. Gleam và các ngôn ngữ hiện đại đơn giản hóa nhiệm vụ này với các hàm tích hợp, như `slice`.

Có lựa chọn khác không? Chắc chắn rồi. Bạn có thể sử dụng so khớp mẫu để phân tách chuỗi, nhưng `slice` thì mượt mà hơn cho việc trích xuất đơn giản.

Về mặt triển khai, `slice` cần phải xem xét mã hóa chuỗi, như UTF-8. Điều này đảm bảo rằng các ký tự, không chỉ là các byte, được trích xuất một cách chính xác mà không làm rối các ký tự nhiều byte. Điều này không phải là dễ dàng trong kỷ nguyên chỉ có ASCII.

## Xem thêm
- Nếu bạn cảm thấy hoài cổ hoặc chỉ đơn giản là tò mò, hãy nhìn vào cách C xử lý chuỗi với các con trỏ: [Xử lý chuỗi C](https://en.cppreference.com/w/c/string/byte)
