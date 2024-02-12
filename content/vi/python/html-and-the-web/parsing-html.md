---
title:                "Phân Tích Cú Pháp HTML"
aliases:
- /vi/python/parsing-html.md
date:                  2024-01-28T22:04:25.047008-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân Tích Cú Pháp HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/python/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?

Việc phân tích cú pháp HTML là quá trình lấy mã HTML và trích xuất thông tin từ nó, giống như việc tìm kim trong đống rơm—nếu như đống rơm được làm từ các thẻ và kim là dữ liệu bạn muốn. Lập trình viên làm điều này để kéo dữ liệu từ các trang web, có thể là mọi thứ từ tiêu đề trên một trang tin tức đến giá cả trong một cửa hàng trực tuyến.

## Làm Thế Nào:

Hãy sử dụng Python để "cướp" một số dữ liệu từ một mẫu HTML bằng cách sử dụng thư viện `BeautifulSoup`, làm cho việc phân tích cú pháp trở nên dễ dàng. Nếu bạn chưa cài đặt gói, hãy cài đặt bằng `pip install beautifulsoup4`.

```Python
from bs4 import BeautifulSoup

# Hãy tưởng tượng đây là HTML của bạn
html_doc = """
<html>
<head>
    <title>Câu chuyện của Chú Chuột đồng</title>
</head>
<body>
    <p class="title">
        <b>Câu chuyện của Chú Chuột đồng</b>
    </p>
    <p class="story">Ngày xửa ngày xưa có ba chị em gái; và tên của họ là
        <a href="http://example.com/elsie" class="sister" id="link1">Elsie</a>,
        <a href="http://example.com/lacie" class="sister" id="link2">Lacie</a> và
        <a href="http://example.com/tillie" class="sister" id="link3">Tillie</a>;
        và họ sống dưới đáy một giếng.</p>
</body>
</html>
"""

# Tạo Soup
soup = BeautifulSoup(html_doc, 'html.parser')

# Tìm thẻ tiêu đề
title_tag = soup.title
print("Tiêu đề của câu chuyện:", title_tag.string)

# Tìm tất cả thẻ 'a' với class là 'sister'
sister_tags = soup.find_all('a', class_='sister')
print("Tên và đường dẫn URL của các chị em:")
for sister in sister_tags:
    print(f"- Tên: {sister.string}, URL: {sister['href']}")
```

Kết quả sẽ là:

```
Tiêu đề của câu chuyện: Câu chuyện của Chú Chuột đồng
Tên và đường dẫn URL của các chị em:
- Tên: Elsie, URL: http://example.com/elsie
- Tên: Lacie, URL: http://example.com/lacie
- Tên: Tillie, URL: http://example.com/tillie
```

## Sâu Hơn Nữa

Trong những ngày đầu của web, bạn sẽ phân tích cú pháp HTML bằng regex và rất nhiều hy vọng. Điều này rất lộn xộn bởi vì HTML không phải lúc nào cũng gọn gàng và dễ đoán. Vào cuộc, các thư viện như BeautifulSoup, điều hướng cấu trúc cây của HTML, cung cấp một cách nhẹ nhàng để cắt và chia dữ liệu.

Cũng có các phương tiện thay thế như `lxml` và `html.parser`, mà chính BeautifulSoup có thể sử dụng như là bộ phân tích cú pháp. `lxml` nhanh hơn nhưng kém tha thứ với HTML xấu, trong khi `html.parser` chậm hơn nhưng không soi mói về các thẻ hỏng.

Phía sau, những thư viện này xây dựng một cây phân tích cú pháp, biến các thẻ thành những đối tượng mà bạn có thể tương tác. BeautifulSoup như một giao diện thân thiện với những bộ phân tích cú pháp này, dịch các câu hỏi của bạn—như "Tiêu đề là gì?" hoặc "Có liên kết nào ở đây không?"—thành các hành động trên cây.

## Xem Thêm

- Tài liệu BeautifulSoup: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Giới thiệu về phân tích cú pháp HTML bằng regex (và tại sao bạn không nên làm điều đó): https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags
- Web scraping với Python (hướng dẫn thực hành): https://realpython.com/beautiful-soup-web-scraper-python/
