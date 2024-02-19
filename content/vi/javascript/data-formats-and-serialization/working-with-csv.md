---
aliases:
- /vi/javascript/working-with-csv/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:46.825293-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi CSV (Comma-Separated Values - Gi\xE1 tr\u1ECB\
  \ t\xE1ch b\u1EB1ng d\u1EA5u ph\u1EA9y) trong JavaScript th\u01B0\u1EDDng c\xF3\
  \ ngh\u0129a l\xE0 ph\xE2n t\xEDch v\xE0 t\u1EA1o d\u1EEF li\u1EC7u v\u0103n b\u1EA3\
  n cho b\u1EA3ng t\xEDnh\u2026"
lastmod: 2024-02-18 23:08:51.160809
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi CSV (Comma-Separated Values - Gi\xE1 tr\u1ECB\
  \ t\xE1ch b\u1EB1ng d\u1EA5u ph\u1EA9y) trong JavaScript th\u01B0\u1EDDng c\xF3\
  \ ngh\u0129a l\xE0 ph\xE2n t\xEDch v\xE0 t\u1EA1o d\u1EEF li\u1EC7u v\u0103n b\u1EA3\
  n cho b\u1EA3ng t\xEDnh\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Làm việc với CSV (Comma-Separated Values - Giá trị tách bằng dấu phẩy) trong JavaScript thường có nghĩa là phân tích và tạo dữ liệu văn bản cho bảng tính hoặc truyền dữ liệu. Lập trình viên làm điều này bởi vì CSV rất phổ biến, nhẹ và dễ đọc hoặc tạo.

## Cách thực hiện:

**Phân tích CSV thành JSON:**
```javascript
const csv = `name,age,city
Alice,30,New York
Bob,22,Los Angeles`;

function csvToJson(csv) {
  const lines = csv.split("\n");
  const headers = lines[0].split(",");
  return lines.slice(1).map(dòng => {
    const data = dòng.split(",");
    return headers.reduce((obj, nextKey, index) => {
      obj[nextKey] = data[index];
      return obj;
    }, {});
  });
}

console.log(csvToJson(csv));
// Kết quả: [{name: 'Alice', age: '30', city: 'New York'}, {name: 'Bob', age: '22', city: 'Los Angeles'}]
```

**Tạo CSV từ JSON:**
```javascript
const jsonData = [
  { name: "Alice", age: 30, city: "New York" },
  { name: "Bob", age: 22, city: "Los Angeles" }
];

function jsonToCsv(json) {
  const headers = Object.keys(json[0]).join(",");
  const hàng = json.map(obj =>
    Object.values(obj).join(",")
  ).join("\n");
  return `${headers}\n${hàng}`;
}

console.log(jsonToCsv(jsonData));
// Kết quả: name,age,city
//          Alice,30,New York
//          Bob,22,Los Angeles
```

## Sâu hơn

CSV đã xuất hiện từ những ngày đầu của việc tính toán - dễ dàng cho cả máy móc để xử lý và con người để hiểu. Nhưng nó không hoàn hảo. Nếu dữ liệu của bạn phức tạp hoặc lồng ghép, JSON hoặc XML có thể phù hợp hơn. Về thực hiện, việc xử lý CSV trong JavaScript cần có các giải pháp do thiếu thư viện chuẩn cho việc này; tuy nhiên, ngày nay có nhiều thư viện như PapaParse hoặc csv-parser đã làm đơn giản hóa nhiệm vụ này. Ngoài ra, các trường hợp cạnh như ký tự dòng mới trong các trường và mã hóa ký tự có thể làm phức tạp việc xử lý CSV và cần chú ý lập trình cẩn thận.

## Xem thêm

- MDN Web Docs về Fetch API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch (Lấy dữ liệu CSV từ web)
- PapaParse: https://www.papaparse.com/ (Bộ phân tích CSV mạnh mẽ cho trình duyệt)
- RFC 4180: https://tools.ietf.org/html/rfc4180 (Tiêu chuẩn cho các tệp CSV)
