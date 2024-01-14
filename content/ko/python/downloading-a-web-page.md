---
title:                "Python: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

만약 웹 페이지를 다운로드하는 것에 대해 궁금하다면, 다른 프로그래밍 언어보다 파이썬은 웹 스크래핑에 유용한 라이브러리들을 가지고 있기 때문에 좋은 선택이 될 수 있습니다.

## Why

웹 페이지를 다운로드하면 인터넷에서 파일을 디스크로 저장할 수 있습니다.

## How To

파이썬에서 웹 페이지를 다운로드하려면 `requests` 라이브러리를 사용해야 합니다. 다음은 해당 라이브러리를 사용하여 웹 페이지를 다운로드하는 간단한 예제 코드입니다.

```python
import requests

url = "https://www.example.com"
response = requests.get(url)

print(response.text)
```

위 코드에서 `requests.get()` 함수를 사용하여 원하는 웹 페이지의 URL을 전달하고, `response` 변수에 반환 값을 저장합니다. 그 후 `response.text`를 통해 해당 웹 페이지의 HTML 코드를 출력할 수 있습니다.

## Deep Dive

웹 스크래핑을 할 때는 `headers` 파라미터를 사용하여 웹 사이트의 요청을 보낼 때 위장하는 것이 중요합니다. 이를 통해 웹 사이트에서 차단되는 것을 방지할 수 있습니다. 또한 `BeautifulSoup` 라이브러리를 사용하면 웹 페이지에서 원하는 정보를 쉽게 추출할 수 있습니다.

## See Also

- [10 Minutes to Pandas: 파이썬 웹 스크래핑](https://pandas.pydata.org/docs/getting_started/10min.html#web-scraping)
- [크롤링(Crawling)과 스크래핑(Scraping)의 차이](https://www.geeksforgeeks.org/web-scraping-and-crawling-101/)
- [파이썬 requests 라이브러리 공식 문서](https://requests.readthedocs.io/en/latest/)