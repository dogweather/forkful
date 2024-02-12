---
title:                "HTML 파싱"
aliases: - /ko/bash/parsing-html.md
date:                  2024-02-03T19:11:43.072217-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML 파싱"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜?

HTML 파싱은 HTML 파일의 구조와 내용을 뒤져서 정보를 추출하는 것을 의미합니다. 프로그래머들은 데이터에 접근하거나, 내용을 조작하거나, 웹사이트를 스크래핑하기 위해 이 작업을 수행합니다.

## 어떻게:

Bash는 HTML 파싱을 위한 첫 번째 선택은 아니지만, `grep`, `awk`, `sed` 또는 `lynx` 같은 외부 유틸리티와 같은 도구들로 할 수는 있습니다. 견고함을 위해, 우리는 `libxml2` 패키지에서 `xmllint`를 사용할 것입니다.

```bash
# 필요한 경우 xmllint 설치
sudo apt-get install libxml2-utils

# 샘플 HTML
cat > sample.html <<EOF
<html>
<head>
  <title>Sample Page</title>
</head>
<body>
  <h1>Hello, Bash!</h1>
  <p id="myPara">Bash can read me.</p>
</body>
</html>
EOF

# 제목 파싱
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "제목은: $title"

# ID로 단락 추출
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "단락 내용은: $para"
```

출력:
```
제목은: Sample Page
단락 내용은: Bash can read me.
```

## 깊이 있는 탐구

예전에, 프로그래머들은 HTML을 스캔하기 위해 `grep` 같은 regex 기반 도구를 사용했지만, 그것은 다소 불편했습니다. HTML은 정규적이지 않고, 상황에 따라 다릅니다. 전통적인 도구들은 이를 놓치고 오류가 발생하기 쉽습니다.

대안? 많습니다. Beautiful Soup를 사용하는 Python, DOMDocument를 사용하는 PHP, DOM 파서를 사용하는 JavaScript—HTML의 구조를 이해하도록 설계된 라이브러리를 가진 언어들.

간단한 작업에 대해 bash 스크립트에서 `xmllint`를 사용하는 것은 안정적입니다. 그것은 XML을 이해하며, 그 확장으로 XHTML을 이해합니다. 하지만 일반 HTML은 예측할 수 없을 수 있습니다. 그것은 항상 XML의 엄격한 규칙을 따르지 않습니다. `xmllint`는 HTML을 XML 모델로 강제 적용하는데, 이는 잘 형성된 HTML에는 잘 작동하지만, 지저분한 것들에는 문제가 발생할 수 있습니다.

## 참고

- [W3Schools - HTML DOM 파서](https://www.w3schools.com/xml/dom_intro.asp): HTML DOM을 명확하게 설명합니다.
- [MDN 웹 문서 - XML 파싱 및 직렬화](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML): XHTML에 적용되는 XML 파싱 원칙에 대해 설명합니다.
- [Beautiful Soup 문서](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): HTML 파싱을 위한 Python 라이브러리.
- [libxml2 문서](http://xmlsoft.org/): `xmllint` 및 관련 XML 도구에 대한 세부 정보.
