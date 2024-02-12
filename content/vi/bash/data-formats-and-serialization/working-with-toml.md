---
title:                "Làm việc với TOML"
aliases:
- /vi/bash/working-with-toml/
date:                  2024-01-28T22:11:28.860791-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/bash/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
TOML, viết tắt của Tom's Obvious, Minimal Language, là một định dạng chuỗi hóa dữ liệu. Lập trình viên ưa thích nó vì sự đơn giản và dễ đọc; nó thực sự phù hợp cho các file cấu hình, có bản chất tương tự như YAML nhưng ít rườm rà hơn JSON đối với con người.

## Cách thực hiện:
Đầu tiên, cài đặt `toml-cli` để thao tác với TOML trong Bash. Tiện lợi cho việc đọc hoặc chỉnh sửa các file TOML một cách nhanh chóng.

```Bash
# Cài đặt toml-cli, trợ thủ đắc lực cho các nhiệm vụ TOML
pip install toml-cli

# Giả sử bạn có một file TOML, 'config.toml'
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# Đọc một giá trị
toml get config.toml owner.name
# Kết quả: Tom

# Thiết lập một giá trị
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# Mẹo chuyên nghiệp: Sử dụng dấu ngoặc kép cho các khóa có dấu chấm hoặc ký tự lạ!
```

## Sâu hơn nữa
Xuất phát từ sự không thích những trở ngại của JSON đối với con người, TOML ra đời vào khoảng năm 2013. Tom Preston-Werner, đồng sáng lập GitHub, mong muốn có một thứ gì đó cực kỳ dễ đọc. YAML và INI là những lựa chọn khác nhưng TOML như là sự kết hợp tốt nhất của cả hai.

Với TOML, bạn có thể quản lý dữ liệu lồng nhau và mảng dữ liệu, tránh được những điểm yếu của YAML và dấu ngoặc nhọn của JSON. Bây giờ, TOML đã trở thành lựa chọn hàng đầu cho file cấu hình trong Cargo của Rust, điều này cho thấy sự tăng trưởng của nó trong thế giới phát triển phần mềm. Nó được điều khiển bởi một bộ quy tắc, giữ cho mọi thứ chặt chẽ và được định rõ. Bạn sẽ tìm thấy các bộ phân tích cú pháp trong hầu như bất kỳ ngôn ngữ nào, làm cho nó dễ dàng được tiếp nhận rộng rãi.

## Xem thêm
- Kho chính thức TOML trên GitHub: https://github.com/toml-lang/toml
- toml-cli trên PyPI: https://pypi.org/project/toml-cli/
- So sánh các định dạng chuỗi hóa dữ liệu: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
