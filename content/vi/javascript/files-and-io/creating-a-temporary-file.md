---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:45.404974-07:00
description: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi cho ph\xE9p \u1EE9ng d\u1EE5\
  ng c\u1EE7a b\u1EA1n l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u m\xE0 n\xF3 kh\xF4ng c\u1EA7\
  n m\xE3i m\xE3i. \u0110i\u1EC1u n\xE0y r\u1EA5t ti\u1EC7n l\u1EE3i khi b\u1EA1n\
  \ \u0111ang x\u1EED l\xFD d\u1EEF li\u1EC7u l\u1EDBn, b\u1ED9 nh\u1EDB \u0111\u1EC7\
  m,\u2026"
lastmod: '2024-02-25T18:49:35.525932-07:00'
model: gpt-4-0125-preview
summary: "T\u1EA1o m\u1ED9t t\u1EC7p t\u1EA1m th\u1EDDi cho ph\xE9p \u1EE9ng d\u1EE5\
  ng c\u1EE7a b\u1EA1n l\u01B0u tr\u1EEF d\u1EEF li\u1EC7u m\xE0 n\xF3 kh\xF4ng c\u1EA7\
  n m\xE3i m\xE3i. \u0110i\u1EC1u n\xE0y r\u1EA5t ti\u1EC7n l\u1EE3i khi b\u1EA1n\
  \ \u0111ang x\u1EED l\xFD d\u1EEF li\u1EC7u l\u1EDBn, b\u1ED9 nh\u1EDB \u0111\u1EC7\
  m,\u2026"
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tạo một tệp tạm thời cho phép ứng dụng của bạn lưu trữ dữ liệu mà nó không cần mãi mãi. Điều này rất tiện lợi khi bạn đang xử lý dữ liệu lớn, bộ nhớ đệm, hoặc khi bạn cần một không gian tạm thời được dọn dẹp tự động.

## Cách thực hiện:

Trong JavaScript, hầu hết các thao tác tệp tạm thời đều dựa vào các thư viện bên ngoài. Dưới đây là một ví dụ nhanh sử dụng thư viện `tmp`, mà bạn có thể cài đặt bằng `npm install tmp`.

```javascript
const tmp = require('tmp');

// Tạo một tệp tạm thời
tmp.file((err, path, fd, cleanupCallback) => {
  if (err) throw err;

  console.log(`Đường dẫn tệp: ${path}`);
  // Làm việc với tệp...

  // Khi bạn đã xong, hãy dọn dẹp
  cleanupCallback();
});
```

Kết quả mẫu có thể như sau:

```
Đường dẫn tệp: /tmp/tmp-9Xp2nVn6hB5W.tmp
```

## Sâu hơn

Việc tạo tệp tạm thời có một lịch sử lâu dài trong lĩnh vực máy tính, bắt đầu từ thời điểm bộ nhớ hệ thống bị hạn chế và dữ liệu trung gian cần một chỗ để tồn tại. Trong Node.js, mô-đun `fs` có thể được sử dụng để tạo tệp tạm thời, nhưng nó thiếu các công cụ tạo tệp tạm thời tích hợp sẵn.

Việc sử dụng các thư viện như `tmp` hoặc `tempfile` là khá phổ biến. Chúng tạo ra các tên tệp duy nhất, giảm nguy cơ xung đột tên và thường xử lý việc dọn dẹp một cách tự động. `fs.mkdtemp` cũng có thể hữu ích cho việc tạo một thư mục tạm thời để đặt nhiều tệp tạm thời.

Về bên trong, các thư viện này thường sử dụng các cơ chế bản địa của HĐH để tạo các tệp này một cách an toàn, thường đặt chúng trong thư mục tạm thời được định nghĩa bởi hệ thống. Trên các hệ thống giống Unix, thường là `/tmp`, trong khi Windows sử dụng cái gì đó phức tạp hơn dưới `LocalAppData`.

Khi xử lý tệp tạm thời, hãy nhớ rằng mặc dù chúng là "tạm thời", nhưng việc xử lý không đúng cách có thể dẫn đến các lỗ hổng bảo mật hoặc tệp sót lại làm rối bộ nhớ hệ thống.

## Xem thêm

- [Mô-đun fs của Node.js](https://nodejs.org/api/fs.html) - cho các thao tác tệp thủ công.
- [Gói `tmp` trên npm](https://www.npmjs.com/package/tmp) - một tiện ích cho tệp và thư mục tạm thời.
- [Gói `tempfile` trên npm](https://www.npmjs.com/package/tempfile) - để tạo đường dẫn tệp tạm thời ngẫu nhiên.
- [Thư mục tạm thời của hệ điều hành](https://en.wikipedia.org/wiki/Temporary_folder) - Trang Wikipedia về thư mục tạm thời trong các HĐH khác nhau.
