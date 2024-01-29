---
title:                "Sử dụng biểu thức chính quy"
date:                  2024-01-28T22:09:33.862408-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng biểu thức chính quy"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Biểu thức chính quy (regex) là những mẫu được sử dụng để khớp các tổ hợp ký tự trong chuỗi. Lập trình viên sử dụng chúng để tìm kiếm, xác thực và thao tác với văn bản bởi vì chúng nhanh và hiệu quả.

## Làm thế nào:

Gleam không hỗ trợ regex sẵn, vì thế chúng ta sử dụng module `gleam_otp/re`. Dưới đây là cách làm:

Đầu tiên, thêm `gleam_otp` vào phụ thuộc `rebar.config` của bạn.

```erlang
{deps, [
    {gleam_otp, "0.1.0"}
]}
```

Bây giờ, viết một số mã Gleam để khớp mẫu:

```rust
import gleam/otp/re

fn main() {
  let pattern = "^Hello, (\\w+)!"
  let text = "Hello, World!"
  
  let result = re.run(pattern, text)
  jsonecho(result)
}

fn jsonecho(result) {
  case result {
    Ok(matches) -> 
      case matches {
        [] -> 
          "Không tìm thấy khớp"
        [_, name] -> 
          "Khớp với tên: " <> name
        _ -> 
          "Số lượng khớp không mong đợi"
      }
    Error(_) -> 
      "Mẫu không biên dịch được"
  }
}
```

Chạy nó, và bạn sẽ thấy `Khớp với tên: World`.

## Thảo luận sâu hơn

Regex đã tồn tại từ những năm 1950; việc sử dụng nó trong các công cụ Unix vào những năm 1970 đã củng cố vị thế của nó. Các phương án thay thế bao gồm các hàm chuỗi, nhưng chúng có thể dài dòng. Về mặt triển khai, regex thường được biên dịch thành một máy trạng thái nội bộ, cung cấp tốc độ nhưng tiềm ẩn sự phức tạp khi viết các mẫu phức tạp.

## Xem thêm

- [Regex101](https://regex101.com/) để kiểm tra các mẫu regex trực tuyến.
- [Giới thiệu về Biểu thức Chính Quy](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) để nắm bắt cơ bản.
