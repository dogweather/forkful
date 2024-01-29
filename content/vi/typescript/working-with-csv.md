---
title:                "Làm việc với CSV"
date:                  2024-01-28T22:11:38.942403-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Làm việc với CSV (Giá trị Được Phân Cách bởi Dấu phẩy) có nghĩa là đọc và viết dữ liệu dưới dạng văn bản, trong đó mỗi dòng là một bản ghi dữ liệu và dấu phẩy phân chia mỗi trường. Các lập trình viên sử dụng CSV vì sự đơn giản và được hỗ trợ rộng rãi trên các công cụ đổi dữ liệu.

## Làm thế nào:

Đọc CSV trong TypeScript rất đơn giản với các thư viện như `papaparse`. Để xử lý tệp CSV, trước tiên hãy cài đặt nó:

```bash
npm install papaparse
```

Dưới đây là cách bạn đọc một tệp CSV:

```typescript
import * as fs from 'fs';
import * as Papa from 'papaparse';

const csvFilePath = 'path/to/your/file.csv';
const fileContent = fs.readFileSync(csvFilePath, 'utf8');

Papa.parse(fileContent, {
  complete: (result) => {
    console.log(result.data);
  }
});
```

Để viết CSV, bạn có thể sử dụng `csv-writer`. Cài đặt nó với:

```bash
npm install csv-writer
```

Và sau đó viết vào một tệp CSV như sau:

```typescript
import * as createCsvWriter from 'csv-writer';

const csvWriter = createCsvWriter.createObjectCsvWriter({
  path: 'path/to/your/output.csv',
  header: [
    {id: 'name', title: 'NAME'},
    {id: 'age', title: 'AGE'}
  ]
});

const data = [
  { name: 'John', age: 28 },
  { name: 'Jane', age: 32 }
];

csvWriter.writeRecords(data)
  .then(() => console.log('Dữ liệu đã được viết vào tệp CSV thành công.'));
```

Kết quả trong 'output.csv' sẽ là:

```
NAME,AGE
John,28
Jane,32
```

## Sâu hơn

CSV đã trở thành một tiêu chuẩn trong đổi dữ liệu từ kỷ nguyên máy tính đầu tiên do khả năng đọc và đơn giản của nó. Tuy nhiên, nó không phải không có vấn đề; chẳng hạn, sự thiếu tiêu chuẩn hóa có thể dẫn đến lỗi phân tích. Các lựa chọn thay thế như JSON và XML cung cấp cấu trúc phức tạp và kiểu dữ liệu hơn. Khi thực hiện công cụ phân tích/ghi CSV, cần xem xét mã hóa ký tự và xử lý chính xác các ký tự đặc biệt để tránh lỗi.

## Xem thêm

- Tài liệu `papaparse`: [Papa Parse - Trình phân tích CSV mạnh mẽ](https://www.papaparse.com/)
- Tài liệu `csv-writer`: [CSV Writer - Trình viết tệp CSV cho Node](https://csv.js.org/)
- Để hiểu sâu hơn về mặt kỹ thuật, tài liệu RFC 4180 cung cấp tiêu chuẩn de facto cho các định dạng CSV: [RFC 4180](https://tools.ietf.org/html/rfc4180)
- Để so sánh các định dạng tệp, xem: [JSON vs XML vs CSV](https://www.geeksforgeeks.org/difference-between-json-and-xml/)
