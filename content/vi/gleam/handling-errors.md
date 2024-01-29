---
title:                "Xử lý lỗi"
date:                  2024-01-28T22:02:20.908227-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xử lý lỗi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Xử lý lỗi là việc dự đoán các trường hợp có thể đi sai trong code của bạn và quản lý những tình huống đó một cách mềm dẻo. Lập trình viên làm điều này vì nó giữ cho các ứng dụng ổn định và thân thiện với người dùng, ngay cả khi đối mặt với những điều bất ngờ.

## Cách thực hiện:
Trong Gleam, bạn thường xuyên sử dụng kiểu `Result` để xử lý lỗi. Đó là một enum với hai biến thể: `Ok` (cho thành công) và `Error` (cho thất bại). Dưới đây là một ví dụ đơn giản:

```Gleam
pub fn might_fail(break_it: Bool) -> Result(Int, String) {
  if break_it {
    Error("Rất tiếc! Nó đã hỏng.".to_string())
  } else {
    Ok(42)
  }
}

pub fn main() {
  let result = might_fail(False)
  case result {
    Ok(value) => value
    Error(message) => {
      io.println(message)
      0
    } 
  }
}
```

Nếu bạn chạy `main` với `might_fail(False)`, nó sẽ trả về `42`. Nếu bạn truyền `True`, nó in "Rất tiếc! Nó đã hỏng." và trả về `0`.

## Tìm hiểu sâu
Cách tiếp cận xử lý lỗi của Gleam được ảnh hưởng bởi gốc rễ Erlang của nó. Theo lịch sử, Erlang sử dụng triết lý "để nó sụp đổ", dựa vào các cây giám sát để quản lý sự cố của quy trình. Tuy nhiên, khi bạn viết code Gleam không nằm trong một quy trình được giám sát, như trong một hàm thư viện, bạn sẽ muốn xử lý lỗi một cách rõ ràng.

Các phương án thay thế để sử dụng `Result` bao gồm sử dụng kiểu `Option` cho các trường hợp một cái gì đó có thể là `None` (không có gì) hoặc `Some` (có cái gì đó), nhưng những cái này không chứa thông tin lỗi. Để tín hiệu lỗi qua các ranh giới của quy trình, bạn có thể sử dụng các cơ chế truyền thông điệp của Erlang.

Xử lý lỗi của Gleam phản ánh một phong cách lập trình chức năng, nơi mà các tác dụng phụ (như lỗi) được quản lý bằng kiểu và khớp mẫu, cung cấp sự rõ ràng và dễ dự đoán trong quản lý lỗi.

## Xem thêm
- [Xử lý lỗi của Erlang](http://erlang.org/doc/reference_manual/errors.html)
