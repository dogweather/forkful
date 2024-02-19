---
aliases:
- /vi/fish-shell/working-with-yaml/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:07.756559-07:00
description: "YAML, \"YAML Ain't Markup Language\" (YAML kh\xF4ng ph\u1EA3i l\xE0\
  \ Ng\xF4n ng\u1EEF \u0110\xE1nh d\u1EA5u), l\xE0 m\u1ED9t ti\xEAu chu\u1EA9n m\xE3\
  \ h\xF3a d\u1EEF li\u1EC7u th\xE2n thi\u1EC7n v\u1EDBi con ng\u01B0\u1EDDi, c\xF3\
  \ k\xFD hi\u1EC7u \u01B0u vi\u1EC7t\u2026"
lastmod: 2024-02-18 23:08:51.208945
model: gpt-4-0125-preview
summary: "YAML, \"YAML Ain't Markup Language\" (YAML kh\xF4ng ph\u1EA3i l\xE0 Ng\xF4\
  n ng\u1EEF \u0110\xE1nh d\u1EA5u), l\xE0 m\u1ED9t ti\xEAu chu\u1EA9n m\xE3 h\xF3\
  a d\u1EEF li\u1EC7u th\xE2n thi\u1EC7n v\u1EDBi con ng\u01B0\u1EDDi, c\xF3 k\xFD\
  \ hi\u1EC7u \u01B0u vi\u1EC7t\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

YAML, "YAML Ain't Markup Language" (YAML không phải là Ngôn ngữ Đánh dấu), là một tiêu chuẩn mã hóa dữ liệu thân thiện với con người, có ký hiệu ưu việt hơn ngôn ngữ bảng và ngôn ngữ đánh dấu cho tệp cấu hình và trao đổi dữ liệu. Lập trình viên sử dụng nó vì sự đơn giản và dễ đọc trong các tệp cấu hình, bảng kê triển khai, và các cấu trúc dữ liệu phức tạp hơn.

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
