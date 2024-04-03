---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:59.160555-07:00
description: "TOML l\xE0 \u0111\u1ECBnh d\u1EA1ng t\u1EC7p c\u1EA5u h\xECnh, d\u1EC5\
  \ \u0111\u1ECDc v\xE0 vi\u1EBFt b\u1EDFi con ng\u01B0\u1EDDi, c\u0169ng nh\u01B0\
  \ d\u1EC5 d\xE0ng ph\xE2n t\xEDch v\xE0 t\u1EA1o ra b\u1EDFi m\xE1y m\xF3c. L\u1EAD\
  p tr\xECnh vi\xEAn s\u1EED d\u1EE5ng TOML cho c\xE1c t\u1EC7p\u2026"
lastmod: '2024-03-13T22:44:37.241961-06:00'
model: gpt-4-0125-preview
summary: "TOML l\xE0 \u0111\u1ECBnh d\u1EA1ng t\u1EC7p c\u1EA5u h\xECnh, d\u1EC5 \u0111\
  \u1ECDc v\xE0 vi\u1EBFt b\u1EDFi con ng\u01B0\u1EDDi, c\u0169ng nh\u01B0 d\u1EC5\
  \ d\xE0ng ph\xE2n t\xEDch v\xE0 t\u1EA1o ra b\u1EDFi m\xE1y m\xF3c."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

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
