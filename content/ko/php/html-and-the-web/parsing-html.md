---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:49.120168-07:00
description: "\uBC29\uBC95: HTML\uC744 \uD30C\uC2F1\uD558\uAE30 \uC704\uD574 PHP \uD504\
  \uB85C\uADF8\uB798\uBA38\uB294 \uB0B4\uC7A5 \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\
  \uAC70\uB098 'Simple HTML DOM Parser'\uC640 \uAC19\uC740 \uAC15\uB825\uD55C \uB77C\
  \uC774\uBE0C\uB7EC\uB9AC\uC5D0 \uC758\uC874\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  . \uC5EC\uAE30\uC11C\uB294 PHP\uC758 `DOMDocument`\uC640 Simple HTML DOM Parser\uB97C\
  \ \uC0AC\uC6A9\uD55C \uC608\uC81C\uB97C\u2026"
lastmod: '2024-03-13T22:44:55.353942-06:00'
model: gpt-4-0125-preview
summary: "HTML\uC744 \uD30C\uC2F1\uD558\uAE30 \uC704\uD574 PHP \uD504\uB85C\uADF8\uB798\
  \uBA38\uB294 \uB0B4\uC7A5 \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uAC70\uB098 'Simple\
  \ HTML DOM Parser'\uC640 \uAC19\uC740 \uAC15\uB825\uD55C \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uC5D0 \uC758\uC874\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

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
