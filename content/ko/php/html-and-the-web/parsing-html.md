---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:49.120168-07:00
description: "PHP\uC5D0\uC11C HTML\uC744 \uD30C\uC2F1\uD558\uB294 \uAC83\uC740 HTML\
  \ \uBB38\uC11C\uC5D0\uC11C \uD2B9\uC815 \uC815\uBCF4\uB97C \uCD94\uCD9C\uD558\uB294\
  \ \uACFC\uC815\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uAC1C\uBC1C\uC790\uB4E4\uC740 \uC774\
  \ \uC791\uC5C5\uC744 \uB370\uC774\uD130 \uCD94\uCD9C \uC790\uB3D9\uD654, \uC6F9\
  \ \uC2A4\uD06C\uB798\uD551 \uB610\uB294 \uB2E4\uC591\uD55C \uC6F9 \uD398\uC774\uC9C0\
  \uC758 \uCF58\uD150\uCE20\uB97C \uC790\uC2E0\uC758 \uC560\uD50C\uB9AC\uCF00\uC774\
  \uC158 \uB0B4\uC5D0\uC11C \uD1B5\uD569\uD558\uC5EC \uC218\uB3D9 \uAC1C\uC785 \uC5C6\
  \uC774 \uAE30\uB2A5\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uAE30 \uC704\uD574 \uC218\uD589\
  \uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.353942-06:00'
model: gpt-4-0125-preview
summary: "PHP\uC5D0\uC11C HTML\uC744 \uD30C\uC2F1\uD558\uB294 \uAC83\uC740 HTML \uBB38\
  \uC11C\uC5D0\uC11C \uD2B9\uC815 \uC815\uBCF4\uB97C \uCD94\uCD9C\uD558\uB294 \uACFC\
  \uC815\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uAC1C\uBC1C\uC790\uB4E4\uC740 \uC774 \uC791\
  \uC5C5\uC744 \uB370\uC774\uD130 \uCD94\uCD9C \uC790\uB3D9\uD654, \uC6F9 \uC2A4\uD06C\
  \uB798\uD551 \uB610\uB294 \uB2E4\uC591\uD55C \uC6F9 \uD398\uC774\uC9C0\uC758 \uCF58\
  \uD150\uCE20\uB97C \uC790\uC2E0\uC758 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uB0B4\
  \uC5D0\uC11C \uD1B5\uD569\uD558\uC5EC \uC218\uB3D9 \uAC1C\uC785 \uC5C6\uC774 \uAE30\
  \uB2A5\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uAE30 \uC704\uD574 \uC218\uD589\uD569\uB2C8\
  \uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

## 무엇을, 왜?
PHP에서 HTML을 파싱하는 것은 HTML 문서에서 특정 정보를 추출하는 과정을 말합니다. 개발자들은 이 작업을 데이터 추출 자동화, 웹 스크래핑 또는 다양한 웹 페이지의 콘텐츠를 자신의 애플리케이션 내에서 통합하여 수동 개입 없이 기능을 향상시키기 위해 수행합니다.

## 방법:
HTML을 파싱하기 위해 PHP 프로그래머는 내장 함수를 사용하거나 'Simple HTML DOM Parser'와 같은 강력한 라이브러리에 의존할 수 있습니다. 여기서는 PHP의 `DOMDocument`와 Simple HTML DOM Parser를 사용한 예제를 살펴보겠습니다.

### `DOMDocument` 사용하기:
PHP의 `DOMDocument` 클래스는 DOM 확장의 일부로, HTML 및 XML 문서를 파싱하고 조작할 수 있게 해줍니다. 다음은 `DOMDocument`를 사용하여 HTML 문서에서 모든 이미지를 찾는 간단한 예입니다:

```php
$html = <<<HTML
<!DOCTYPE html>
<html>
<head>
    <title>샘플 페이지</title>
</head>
<body>
    <img src="image1.jpg" alt="이미지 1">
    <img src="image2.jpg" alt="이미지 2">
</body>
</html>
HTML;

$doc = new DOMDocument();
@$doc->loadHTML($html);
$images = $doc->getElementsByTagName('img');

foreach ($images as $img) {
    echo $img->getAttribute('src') . "\n";
}
```

샘플 출력:
```
image1.jpg
image2.jpg
```

### Simple HTML DOM Parser 사용하기:
보다 복잡한 작업을 위해 또는 더 쉬운 문법을 선호한다면, 제3의 라이브러리를 사용할 수 있습니다. Simple HTML DOM Parser는 jQuery와 유사한 인터페이스를 제공하여 HTML 구조를 탐색하고 조작하는 인기 있는 선택입니다. 사용 방법은 다음과 같습니다:

먼저, Composer를 사용하여 라이브러리를 설치하세요:
```
composer require simple-html-dom/simple-html-dom
```

그 다음, 모든 링크를 찾기 위해 HTML을 조작하세요:

```php
require_once 'vendor/autoload.php';

use simplehtmldom\HtmlWeb;

$client = new HtmlWeb();
$html = $client->load('http://www.example.com');

foreach($html->find('a') as $element) {
    echo $element->href . "\n";
}
```

이 코드 스니펫은 'http://www.example.com'의 HTML 내용을 가져와서 파싱한 뒤 모든 하이퍼링크를 출력합니다. 실제로 파싱하고 싶은 URL로 `'http://www.example.com'`을 교체하세요.

이러한 방법을 활용하여, PHP 개발자는 HTML 콘텐츠를 효과적으로 파싱하고, 데이터 추출을 자신의 요구에 맞게 조정하거나, 외부 웹 콘텐츠를 프로젝트에 원활하게 통합할 수 있습니다.
