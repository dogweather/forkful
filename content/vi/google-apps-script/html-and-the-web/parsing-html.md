---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:26.125573-07:00
description: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML trong Google Apps Script bao g\u1ED3\
  m vi\u1EC7c tr\xEDch xu\u1EA5t d\u1EEF li\u1EC7u t\u1EEB n\u1ED9i dung HTML, \u0111\
  i\u1EC1u n\xE0y \u0111\u1EB7c bi\u1EC7t h\u1EEFu \xEDch khi t\u01B0\u01A1ng t\xE1\
  c v\u1EDBi c\xE1c trang\u2026"
lastmod: '2024-03-11T00:14:09.263085-06:00'
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML trong Google Apps Script bao g\u1ED3\
  m vi\u1EC7c tr\xEDch xu\u1EA5t d\u1EEF li\u1EC7u t\u1EEB n\u1ED9i dung HTML, \u0111\
  i\u1EC1u n\xE0y \u0111\u1EB7c bi\u1EC7t h\u1EEFu \xEDch khi t\u01B0\u01A1ng t\xE1\
  c v\u1EDBi c\xE1c trang\u2026"
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Phân tích cú pháp HTML trong Google Apps Script bao gồm việc trích xuất dữ liệu từ nội dung HTML, điều này đặc biệt hữu ích khi tương tác với các trang web hoặc nguồn dữ liệu dựa trên web. Các lập trình viên làm điều này để tự động hóa việc thu thập dữ liệu, thao tác nội dung web hoặc tích hợp chức năng web với các ứng dụng Google như Sheets và Docs.

## Làm sao:
Google Apps Script không có phương thức sẵn có để phân tích cú pháp HTML. Tuy nhiên, bạn có thể tận dụng dịch vụ `UrlFetchApp` để lấy nội dung HTML và sau đó sử dụng các phương thức JavaScript hoặc regex (biểu thức chính quy) để phân tích cú pháp. Dưới đây là một ví dụ cơ bản về cách lấy và phân tích cú pháp thẻ title từ một trang web.

```javascript
function parseHTMLTitle(url) {
  // Lấy nội dung HTML của trang web
  const response = UrlFetchApp.fetch(url);
  const htmlContent = response.getContentText();

  // Sử dụng một regex đơn giản để tìm nội dung của thẻ <title>
  const titleRegex = /<title>(.*?)<\/title>/;
  const match = htmlContent.match(titleRegex);

  // Kiểm tra xem có tìm thấy tiêu đề không và trả về nó
  if (match && match.length > 1) {
    return match[1];
  }

  return 'Không tìm thấy tiêu đề';
}

// Ví dụ sử dụng
const url = 'http://example.com';
const pageTitle = parseHTMLTitle(url);
Logger.log(pageTitle); // Xuất ra tiêu đề của trang web
```

Đối với việc phân tích cú pháp HTML phức tạp hơn, bạn có thể sử dụng `XmlService` để phân tích cú pháp HTML như XML. Lưu ý, tuy nhiên, điều này yêu cầu HTML phải là XML được cấu trúc tốt, điều mà không phải lúc nào cũng đúng:

```javascript
function parseHTMLUsingXmlService(htmlContent) {
  try {
    const document = XmlService.parse(htmlContent);
    const rootElement = document.getRootElement();
    // Từ đây, điều hướng cây XML với các phương thức của XmlService
    // Ví dụ, để tìm một phần tử hoặc thuộc tính cụ thể
  } catch(e) {
    Logger.log('Lỗi khi phân tích cú pháp HTML: ' + e.toString());
  }
}
```

## Đào sâu:
Lịch sử, phân tích cú pháp HTML trong các môi trường như Google Apps Script đã là thách thức do thiếu một Document Object Model (DOM) hoặc các thư viện phân tích cú pháp dành riêng thường thấy trong các bối cảnh lập trình khác. JavaScript trong một trình duyệt, chẳng hạn, có DOM sẵn sàng sử dụng, và các môi trường Node.js có quyền truy cập vào rất nhiều gói NPM như `cheerio` hoặc `jsdom` để phân tích cú pháp HTML.

Cách tiếp cận của Google Apps Script phụ thuộc nhiều vào việc sử dụng `UrlFetchApp` để yêu cầu web và sau đó thao tác dữ liệu phản hồi bằng cách sử dụng phương pháp phân tích cú pháp bằng regex hoặc XML. Mặc dù regex có thể hữu ích cho các nhiệm vụ phân tích cú pháp đơn giản, nói chung nó không nên được khuyến khích cho HTML phức tạp do nguy cơ lỗi và bản chất có thể dễ vỡ của mã. Phân tích cú pháp XML với `XmlService` cung cấp một cách tiếp cận có cấu trúc hơn nhưng đòi hỏi HTML/XML được cấu trúc tốt, có thể là một hạn chế khi xử lý các trang web tuỳ ý.

Đối với nhu cầu phân tích cú pháp phức tạp hoặc khi xử lý HTML kém cấu trúc, một chiến lược thay thế có thể bao gồm sử dụng một dịch vụ web ngoài Google Apps Script. Dịch vụ này có thể xử lý nội dung HTML, có thể sử dụng kỹ thuật hoặc thư viện phân tích cú pháp mạnh mẽ hơn, và sau đó trả lại dữ liệu đã được xử lý dưới dạng dễ tiêu thụ bởi Google Apps Script. Tuy nhiên, cách tiếp cận này giới thiệu độ trễ mạng và độ phức tạp của việc quản lý một dịch vụ web bổ sung.

Dù có những thách thức, việc phân tích cú pháp HTML trong Google Apps Script vẫn là một công cụ mạnh mẽ, đặc biệt khi kết hợp với các dịch vụ và API khác của Google, cung cấp một loạt khả năng tự động hóa có thể tăng cường đáng kể năng suất và khả năng xử lý dữ liệu.
