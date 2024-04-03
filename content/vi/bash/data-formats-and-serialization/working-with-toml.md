---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:28.860791-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: \u0110\u1EA7u ti\xEAn, c\xE0i \u0111\u1EB7\
  t `toml-cli` \u0111\u1EC3 thao t\xE1c v\u1EDBi TOML trong Bash. Ti\u1EC7n l\u1EE3\
  i cho vi\u1EC7c \u0111\u1ECDc ho\u1EB7c ch\u1EC9nh s\u1EEDa c\xE1c file TOML m\u1ED9\
  t c\xE1ch nhanh ch\xF3ng."
lastmod: '2024-03-13T22:44:36.907975-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EA7u ti\xEAn, c\xE0i \u0111\u1EB7t `toml-cli` \u0111\u1EC3 thao\
  \ t\xE1c v\u1EDBi TOML trong Bash."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

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
