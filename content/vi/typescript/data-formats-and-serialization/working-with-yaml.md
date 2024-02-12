---
title:                "Làm việc với YAML"
aliases:
- /vi/typescript/working-with-yaml.md
date:                  2024-01-28T22:12:06.639520-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

YAML là một chuẩn hóa dữ liệu dễ đọc với con người. Lập trình viên sử dụng nó cho các tệp cấu hình, trao đổi dữ liệu giữa các ngôn ngữ, và hơn nữa bởi vì nó đơn giản và dễ đọc.

## Cách thực hiện:

Để làm việc với YAML trong TypeScript, bạn sẽ cần một thư viện như `js-yaml`. Trước tiên, hãy cài đặt nó:

```bash
npm install js-yaml
```

Bây giờ, chuyển một chuỗi YAML thành một đối tượng JavaScript:

```typescript
import yaml from 'js-yaml';

const yamlStr = `
name: John Doe
age: 30
`;

try {
  const doc = yaml.load(yamlStr);
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

Đầu ra mẫu:

```json
{ name: 'John Doe', age: 30 }
```

Để chuyển đổi một đối tượng thành một chuỗi YAML:

```typescript
import yaml from 'js-yaml';

const obj = { name: 'Jane Doe', age: 25 };

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

Đầu ra mẫu:

```yaml
name: Jane Doe
age: 25
```

## Sâu hơn nữa

YAML được bắt đầu vào năm 2001, nhằm mục tiêu đọc và trao đổi dữ liệu dễ dàng giữa các ngôn ngữ. Nó là một tập mở rộng của JSON. Các lựa chọn thay thế bao gồm JSON và XML, nhưng cú pháp tối giản của YAML thường được ưu tiên cho các tệp cấu hình. Khi bạn làm việc với YAML trong TypeScript, hãy nhớ rằng nó không có kiểu dữ liệu; hãy cẩn thận với dữ liệu nhận được, đặc biệt là từ các nguồn không đáng tin cậy, để tránh các vấn đề về bảo mật.

## Xem thêm

- Trang web chính thức của YAML: http://yaml.org
- Kho lưu trữ `js-yaml` trên GitHub: https://github.com/nodeca/js-yaml
- So sánh YAML và JSON: https://en.wikipedia.org/wiki/YAML#Comparison_with_JSON
