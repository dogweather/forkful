---
title:                "Làm việc với TOML"
aliases:
- /vi/fish-shell/working-with-toml/
date:                  2024-01-28T22:10:59.160555-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
TOML là định dạng tệp cấu hình, dễ đọc và viết bởi con người, cũng như dễ dàng phân tích và tạo ra bởi máy móc. Lập trình viên sử dụng TOML cho các tệp cấu hình phân cấp, rõ ràng trong các dự án nơi mà tính dễ đọc là chìa khóa.

## Làm thế nào:
Để đọc và thao tác với TOML trong Fish, bạn có thể sử dụng một công cụ như `yj`, có thể chuyển đổi TOML sang JSON. Dưới đây là cách làm:

```fish
# Cài đặt yj qua Fisher
fisher install jorgebucaran/yj

# Chuyển đổi TOML sang JSON
echo 'title = "Ví dụ TOML"' | yj -tj

# Kết quả mẫu
{"title":"Ví dụ TOML"}
```

Để viết TOML, bạn đảo ngược quá trình:

```fish
# Chuyển đổi JSON sang TOML
echo '{"title":"Ví dụ JSON"}' | yj -jt

# Kết quả mẫu
title = "Ví dụ JSON"
```

Đối với các tác vụ nặng, hãy xem xét sử dụng một công cụ CLI TOML chuyên dụng như `toml-cli`.

```fish
# Cài đặt toml-cli
pip install toml-cli

# Thiết lập một giá trị trong tệp TOML
toml set pyproject.toml tool.poetry.version "1.1.4"

# Lấy một giá trị từ tệp TOML
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## Sâu hơn
TOML (Ngôn Ngữ Tối Giản, Rõ Ràng của Tom), được giới thiệu bởi Tom Preston-Werner vào năm 2013, tương tự như INI nhưng với một đặc điểm kỹ thuật xác định và hệ thống phân cấp dữ liệu. JSON và YAML là những lựa chọn thay thế chính, nhưng chúng có những điểm đánh đổi: JSON không thân thiện với người dùng như TOML, trong khi YAML phức tạp hơn. Thiết kế của TOML phát huy hiệu quả trong các tình huống mà tệp cấu hình thường được duy trì bằng tay, cân bằng giữa sự đơn giản và biểu đạt. Khi nói đến việc triển khai, các trình phân tích TOML có sẵn cho hầu hết các ngôn ngữ lập trình, bao gồm TomlBombadil cho Fish có thể được tích hợp ngay vào trong các kịch bản của bạn.

## Tham khảo thêm
- Đặc điểm chính thức của TOML: https://toml.io
- `yj`, một công cụ để chuyển đổi giữa TOML, JSON, YAML, và XML: https://github.com/jorgebucaran/yj
- `toml-cli`, một tiện ích dòng lệnh cho TOML: https://github.com/sdispater/toml-cli
