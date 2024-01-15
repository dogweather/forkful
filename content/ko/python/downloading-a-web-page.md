---
title:                "웹 페이지 다운로드하기"
html_title:           "Python: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜?

웹 페이지를 다운로드하는 이유는 여러 가지가 있지만, 가장 일반적인 이유는 특정 웹사이트에서 정보를 수집하고자 할 때입니다. 예를 들어, 온라인 쇼핑을 위해 제품 정보를 수집하거나, 뉴스 기사를 읽을 때 사용하게 됩니다.

## 다운로드하는 방법

```Python
# requests 모듈 import
import requests

# 다운로드할 웹 페이지의 URL
url = "https://www.example.com"

# get 메서드를 사용하여 웹 페이지 다운로드
r = requests.get(url)

# 다운로드한 웹 페이지의 HTML 코드 출력
print(r.text)
```

**출력:**

```html
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
</head>

<body>
    <h1>This is an example page</h1>
    <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed euismod ex rutrum tortor convallis, quis sollicitudin dui viverra. Vivamus ut neque non dui dignissim tempor a id sapien. </p>
</body
</html>
```

위 코드는 requests 모듈을 사용하여 웹 페이지를 다운로드하고, 그결과를 HTML 코드로 출력하는 예제입니다. 이를 통해 간단하게 웹 페이지를 다운로드 할 수 있습니다.

## 더 깊게 알아보기

웹 페이지를 다운로드하는 방법은 여러 가지가 있지만, 가장 일반적인 방법은 requests 모듈을 사용하는 것입니다. 이 모듈은 HTTP 요청을 보내고 응답을 받는 기능을 제공하는 간단하고 사용하기 쉬운 모듈입니다. 다만, requests 모듈을 사용하기 전에는 다른 모듈들과 마찬가지로 설치가 필요합니다.

## 관련 자료

- [Requests: HTTP for Humans](https://requests.readthedocs.io/en/master/)
- [Python Web Scraping Tutorial using BeautifulSoup](https://www.datacamp.com/community/tutorials/web-scraping-using-python)

See Also:

> 관련 자료:
> - [Requests: HTTP for Humans](https://requests.readthedocs.io/en/master/)
> - [Python Web Scraping Tutorial using BeautifulSoup](https://www.datacamp.com/community/tutorials/web-scraping-using-python)