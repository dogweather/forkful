---
title:                "HTML 구문 분석"
html_title:           "PHP: HTML 구문 분석"
simple_title:         "HTML 구문 분석"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/parsing-html.md"
---

{{< edit_this_page >}}

# 파싱 HTML이란 무엇이며, 왜 프로그래머들이 이를 하는 걸까?

파싱 HTML은 웹 사이트에서 정보를 추출하는 것을 말합니다. 정보를 추출하는 이유는 데이터를 쉽게 얻고 가공하기 위해서입니다. 예를 들어, 웹 사이트에서 제공하는 데이터를 이용하여 자동으로 정보를 수집하거나 분석할 수 있습니다.

# 어떻게 하면 될까?

PHP에서 HTML을 파싱하는 방법은 간단합니다. 먼저, `file_get_contents()` 함수를 이용하여 웹 사이트 주소에서 데이터를 가져옵니다. 그리고 `preg_match()` 함수를 이용하여 해당 데이터에서 원하는 정보를 정규식을 이용하여 추출합니다. 예제 코드와 출력 결과는 아래와 같습니다.

```
<?php
$html = file_get_contents("https://www.example.com");
preg_match("/<h1>(.*?)<\/h1>/", $html, $matches);
echo "사이트 제목: " . $matches[1];
?>
```

출력 결과:

```
사이트 제목: Example Website
```

# 더 깊이 알아보기

## 역사적 맥락

HTML 파싱은 웹 개발이 시작된 초기부터 사용되어 온 기술입니다. 초기에는 많은 사람들이 직접 HTML 문서를 파싱하고 정보를 추출했지만, 이는 많은 시간과 노력을 요구했습니다. 따라서 파싱 기술이 등장하면서 이를 자동화하는 방법을 개발했고, 지금에 이르러 많은 개발자가 이를 이용하여 자동으로 정보를 추출하고 가공합니다.

## 대안

HTML 파싱을 하는 데 PHP 이외에도 다양한 도구들이 존재합니다. 예를 들어, Python에는 BeautifulSoup 많이 사용되는 HTML 파싱 라이브러리가 있고, JavaScript에는 Cheerio와 jQuery가 있습니다. 각 언어에 따라 장단점이 있으니 용도에 맞게 선택하면 됩니다.

## 구현 상세 정보

HTML 파싱을 할 때 정규식을 이용하는 것이 일반적입니다. 그 이유는 HTML의 형식이 매우 유연하기 때문입니다. 다른 방법으로는 DOM(Document Object Model)을 이용하는 것도 있습니다. 그러나 이는 HTML의 형식이 변경되거나 잘못된 형식의 HTML일 경우 문제를 일으킬 수 있습니다.

# 관련 정보

* PHP `file_get_contents()` 함수 문서: https://www.php.net/manual/en/function.file-get-contents.php
* PHP `preg_match()` 함수 문서: https://www.php.net/manual/en/function.preg-match.php
* BeautifulSoup: https://www.crummy.com/software/BeautifulSoup/
* Cheerio: https://cheerio.js.org/
* jQuery: https://jquery.com/