---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:43.072217-07:00
description: "\uC5B4\uB5BB\uAC8C: Bash\uB294 HTML \uD30C\uC2F1\uC744 \uC704\uD55C\
  \ \uCCAB \uBC88\uC9F8 \uC120\uD0DD\uC740 \uC544\uB2C8\uC9C0\uB9CC, `grep`, `awk`,\
  \ `sed` \uB610\uB294 `lynx` \uAC19\uC740 \uC678\uBD80 \uC720\uD2F8\uB9AC\uD2F0\uC640\
  \ \uAC19\uC740 \uB3C4\uAD6C\uB4E4\uB85C \uD560 \uC218\uB294 \uC788\uC2B5\uB2C8\uB2E4\
  . \uACAC\uACE0\uD568\uC744 \uC704\uD574, \uC6B0\uB9AC\uB294 `libxml2` \uD328\uD0A4\
  \uC9C0\uC5D0\uC11C `xmllint`\uB97C \uC0AC\uC6A9\uD560 \uAC83\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.478859-06:00'
model: gpt-4-0125-preview
summary: "Bash\uB294 HTML \uD30C\uC2F1\uC744 \uC704\uD55C \uCCAB \uBC88\uC9F8 \uC120\
  \uD0DD\uC740 \uC544\uB2C8\uC9C0\uB9CC, `grep`, `awk`, `sed` \uB610\uB294 `lynx`\
  \ \uAC19\uC740 \uC678\uBD80 \uC720\uD2F8\uB9AC\uD2F0\uC640 \uAC19\uC740 \uB3C4\uAD6C\
  \uB4E4\uB85C \uD560 \uC218\uB294 \uC788\uC2B5\uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

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
