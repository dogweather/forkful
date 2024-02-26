---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:59.982289-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi YAML c\xF3 ngh\u0129a l\xE0 ph\xE2n t\xED\
  ch v\xE0 t\u1EA1o ra c\xE1c t\xE0i li\u1EC7u YAML (Yet Another Markup Language -\
  \ Ng\xF4n Ng\u1EEF \u0110\xE1nh D\u1EA5u Kh\xE1c) trong Python. L\u1EADp tr\xEC\
  nh vi\xEAn\u2026"
lastmod: '2024-02-25T18:49:34.499756-07:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi YAML c\xF3 ngh\u0129a l\xE0 ph\xE2n t\xEDch v\xE0\
  \ t\u1EA1o ra c\xE1c t\xE0i li\u1EC7u YAML (Yet Another Markup Language - Ng\xF4\
  n Ng\u1EEF \u0110\xE1nh D\u1EA5u Kh\xE1c) trong Python. L\u1EADp tr\xECnh vi\xEA\
  n\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
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
