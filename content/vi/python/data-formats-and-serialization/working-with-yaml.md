---
title:                "Làm việc với YAML"
aliases: - /vi/python/working-with-yaml.md
date:                  2024-01-28T22:11:59.982289-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Làm việc với YAML có nghĩa là phân tích và tạo ra các tài liệu YAML (Yet Another Markup Language - Ngôn Ngữ Đánh Dấu Khác) trong Python. Lập trình viên thực hiện điều này để quản lý các tệp cấu hình, cài đặt ứng dụng, hoặc việc chuỗi hóa dữ liệu dễ đọc và viết cho con người.

## Làm thế nào:
Để làm việc với YAML trong Python, bạn cần `pyyaml`. Cài đặt nó sử dụng:

```Python
pip install pyyaml
```

Đọc một tệp YAML:

```Python
import yaml

with open('config.yaml', 'r') as stream:
    try:
        config = yaml.safe_load(stream)
        print(config)
    except yaml.YAMLError as exc:
        print(exc)
```

Viết vào một tệp YAML:

```Python
config = {'database': {'host': 'localhost', 'port': 3306}}

with open('config.yaml', 'w') as file:
    yaml.dump(config, file, default_flow_style=False)
```

Đây là nội dung của `config.yaml`:

```yaml
database:
  host: localhost
  port: 3306
```

## Đào Sâu
YAML được ra mắt vào năm 2001 như một tiêu chuẩn chuỗi hóa dữ liệu thân thiện với con người cho tất cả ngôn ngữ lập trình. JSON và XML là các lựa chọn khác nhau, nhưng điểm nổi bật của YAML là sự tập trung vào khả năng đọc. Khi phân tích, `safe_load` là rất quan trọng để ngăn chặn việc thực thi mã tùy ý do nội dung YAML không an toàn. `default_flow_style=False` giữ cho đầu ra không giống JSON, bảo toàn tính dễ đọc của YAML.

## Xem thêm
- Tài liệu PyYAML Chính thức: https://pyyaml.org/wiki/PyYAMLDocumentation
- Thông số kỹ thuật YAML: https://yaml.org/spec/1.2/spec.html
- So sánh giữa JSON và YAML: https://csrc.nist.gov/csrc/media/projects/cryptographic-standards-and-guidelines/documents/examples/data-serialization.pdf
