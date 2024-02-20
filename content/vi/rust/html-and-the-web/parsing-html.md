---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:19.871187-07:00
description: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML l\xE0 h\xE0nh \u0111\u1ED9ng l\u1EA5\
  y m\u1ED9t chu\u1ED7i HTML v\xE0 ph\xE2n t\xE1ch n\xF3 th\xE0nh m\u1ED9t c\u1EA5\
  u tr\xFAc d\u1EEF li\u1EC7u m\xE0 ch\u01B0\u01A1ng tr\xECnh c\u1EE7a b\u1EA1n c\xF3\
  \ th\u1EC3 hi\u1EC3u v\xE0 thao t\xE1c. L\u1EADp\u2026"
lastmod: 2024-02-19 22:04:55.524993
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML l\xE0 h\xE0nh \u0111\u1ED9ng l\u1EA5\
  y m\u1ED9t chu\u1ED7i HTML v\xE0 ph\xE2n t\xE1ch n\xF3 th\xE0nh m\u1ED9t c\u1EA5\
  u tr\xFAc d\u1EEF li\u1EC7u m\xE0 ch\u01B0\u01A1ng tr\xECnh c\u1EE7a b\u1EA1n c\xF3\
  \ th\u1EC3 hi\u1EC3u v\xE0 thao t\xE1c. L\u1EADp\u2026"
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
---

{{< edit_this_page >}}

## Gì và Tại Sao?

Phân tích cú pháp HTML là hành động lấy một chuỗi HTML và phân tách nó thành một cấu trúc dữ liệu mà chương trình của bạn có thể hiểu và thao tác. Lập trình viên làm điều này để tương tác với nội dung web, trích xuất thông tin và tự động hóa các nhiệm vụ liên quan đến web.

## Làm Thế Nào:

Để phân tích cú pháp HTML trong Rust, bạn sẽ muốn sử dụng một crate như `scraper` hoặc `select`. Sau đây là một ví dụ nhanh sử dụng `scraper`:

```Rust
use scraper::{Html, Selector};

fn main() {
    // HTML dưới dạng chuỗi nhập vào
    let html = r#"
        <html>
            <body>
                <p>Xin chào, thế giới!</p>
            </body>
        </html>
    "#;

    // Phân tích cú pháp chuỗi HTML
    let document = Html::parse_document(html);
    
    // Tạo một bộ chọn để tìm tất cả các thẻ <p>
    let selector = Selector::parse("p").unwrap();

    // Lặp qua các phần tử phù hợp với bộ chọn
    for element in document.select(&selector) {
        // In ra văn bản bên trong mỗi thẻ <p>
        println!("{}", element.text().collect::<Vec<_>>().concat());
    }
}
```

Kết Quả:
```
Xin chào, thế giới!
```

## Sâu Hơn

Trước kia, việc phân tích cú pháp HTML là một việc rối rắm. Thư viện thay đổi, tiêu chuẩn không ổn định, và ngôn ngữ có cách tiếp cận khác nhau. Ngày nay, hệ sinh thái của Rust cung cấp các crates vững chắc cho việc phân tích cú pháp, như `scraper` được hỗ trợ bởi các thư viện `html5ever` và `selectors`. `html5ever` đặc biệt thú vị; nó dựa trên thuật toán phân tích cú pháp HTML được quy định bởi WHATWG, khiến nó ngang hàng với cách các trình duyệt hiện đại phân tích HTML.

Các phương án thay thế cho `scraper` bao gồm `select`, mang lại chức năng tương tự nhưng có tính ergonomic khác. Phân tích cú pháp cấp thấp là khả thi với chính `html5ever` nếu bạn cần nhiều kiểm soát hơn.

Thường xuyên, phân tích cú pháp HTML là một phần của việc web scraping, nơi bạn trích xuất dữ liệu từ các website. Quan trọng (và có đạo đức) khi tôn trọng `robots.txt` và điều khoản dịch vụ của trang web khi scraping.

Về mặt triển khai, luôn nhớ rằng phân tích cú pháp chỉ là điểm khởi đầu. Sự sàng lọc và xác thực là chìa khóa để tránh các vấn đề an ninh như tấn công XSS (Cross-Site Scripting), đặc biệt nếu bạn dự định hiển thị hoặc lưu trữ dữ liệu phân tích.

## Xem Thêm

- Crate `scraper`: https://crates.io/crates/scraper
- Crate `select`: https://crates.io/crates/select
- GitHub repo của `html5ever`: https://github.com/servo/html5ever
- Phần "Web scraping" của Rust Cookbook: https://rust-lang-nursery.github.io/rust-cookbook/web/scraping.html
- Đặc tả phân tích cú pháp HTML của WHATWG: https://html.spec.whatwg.org/multipage/parsing.html
- Hướng dẫn về xử lý lỗi của Rust: https://doc.rust-lang.org/book/ch09-00-error-handling.html (để xử lý các sự cố tiềm ẩn của `unwrap`)
