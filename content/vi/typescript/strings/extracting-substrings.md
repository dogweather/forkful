---
title:                "Trích xuất chuỗi con"
aliases:
- /vi/typescript/extracting-substrings.md
date:                  2024-01-28T22:00:23.120298-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trích xuất chuỗi con"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/typescript/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Trích xuất chuỗi con có nghĩa là lấy ra các phần cụ thể của một chuỗi. Điều này rất tiện lợi cho các công việc như phân tích dữ liệu, xác thực đầu vào, hoặc chỉ đơn giản là phân chia văn bản thành các phần dễ quản lý hơn.

## Làm thế nào:
Trong TypeScript, bạn có thể cắt và chọn các chuỗi với các phương thức như `substring()`, `slice()`, và `includes()` của ES6 để tìm kiếm văn bản trong chuỗi.

```TypeScript
let fullString: string = "Hello, TypeScript enthusiasts!";

// Lấy từ ký tự thứ 7 đến 18
let substr: string = fullString.substring(7, 18);
console.log(substr); // In ra: TypeScript

// Tương tự nhưng với slice()
let sliced: string = fullString.slice(7, 18);
console.log(sliced); // In ra: TypeScript

// Kiểm tra xem một chuỗi con có tồn tại không
let exists: boolean = fullString.includes("TypeScript");
console.log(exists); // In ra: true
```

## Đi sâu vào vấn đề
Ngày xửa ngày xưa, việc thao tác với chuỗi thường khó khăn hơn—nghĩ đến các hàm xử lý chuỗi của C. Bây giờ, JavaScript và TypeScript cung cấp các phương pháp xử lý Unicode, tôn trọng mã hóa ký tự, và làm việc trực tiếp với các đối tượng chuỗi. `substring()` và `slice()` tương tự nhau nhưng có một điểm khác biệt: `slice()` có thể nhận chỉ số âm, tính ngược từ cuối. `substring()` coi chúng là số không. Trong các tình huống cần xem xét hiệu năng, việc chọn lựa giữa chúng có thể quan trọng, nhưng cho việc sử dụng hàng ngày, điều này khá là tương đương.

```TypeScript
// Sử dụng chỉ số âm với slice
let endSliced: string = fullString.slice(-25, -7);
console.log(endSliced); // In ra: Hello, Type
```

Đối với `includes()`, nó là một lợi ích cho tính dễ đọc hơn so với `indexOf()` truyền thống, giúp ý định của bạn trở nên rõ ràng ngay từ cái nhìn đầu tiên. Không cần phải `if (string.indexOf('một số văn bản') !== -1)` nữa; chỉ cần sử dụng đơn giản `if (string.includes('một số văn bản'))`.

## Xem thêm
- Sổ tay TypeScript về chuỗi, để biết thêm về cách sử dụng kiểu `'string'`: [Chuỗi TypeScript](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string)
- MDN Web Docs về các phương pháp Chuỗi trong JavaScript, áp dụng cho TypeScript: [MDN Chuỗi](https://developer.mozilla.org/vi/docs/Web/JavaScript/Reference/Global_Objects/String)
- Để hiểu thêm về Unicode và JavaScript (do đó là TypeScript), hãy xem [Hiểu về mã hóa ký tự nội bộ của JavaScript: UCS-2? UTF-16?](http://mathiasbynens.be/notes/javascript-encoding)
