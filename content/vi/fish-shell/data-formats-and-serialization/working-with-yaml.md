---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:07.756559-07:00
description: ''
lastmod: '2024-04-05T21:59:50.152931-06:00'
model: gpt-4-0125-preview
summary: ''
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

## Cách thực hiện:


### Đọc cấu hình YAML
```Fish Shell
# Giả sử 'config.yaml' chứa:
# name: Fishy
# occupation: Shell

set config (yaml2json < config.yaml | jq -r '.name, .occupation')
echo $config
# Kết quả: Fishy Shell
```

### Ghi vào tệp YAML
```Fish Shell
# Sử dụng 'yq', một trình xử lý YAML dòng lệnh di động
echo -e "name: Nemo\ncolor: Orange" > fish.yaml

# Thêm một key mới
yq e '.friends += ["Dory"]' -i fish.yaml

cat fish.yaml
# Kết quả:
# name: Nemo
# color: Orange
# friends:
# - Dory
```

## Sâu hơn nữa
YAML xuất hiện vào đầu những năm 2000 như một sự đơn giản hóa của XML và kể từ đó đã trở thành tiêu chuẩn cho các tệp cấu hình trong ngành công nghiệp phần mềm. Cú pháp tối thiểu của nó vừa là một ưu điểm vừa là một nhược điểm—dễ đọc nhưng khó phân tích cú pháp mà không cần đến thư viện. Các lựa chọn thay thế cho YAML bao gồm JSON, XML và TOML, mỗi cái đều có những điều đánh đổi ứng dụng của riêng nó. Trong Fish Shell, `yq` và `yaml2json` thường được sử dụng để thao tác với các tệp YAML vì Fish không hỗ trợ phân tích cú pháp YAML tích hợp.

## Xem thêm
- Trang chính thức của YAML: https://yaml.org
- Hướng dẫn sử dụng `jq`: https://stedolan.github.io/jq/manual/
- Kho lưu trữ và tài liệu `yq`: https://github.com/mikefarah/yq
