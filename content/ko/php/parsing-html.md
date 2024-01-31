---
title:                "HTML 파싱"
date:                  2024-01-20T15:33:04.545803-07:00
simple_title:         "HTML 파싱"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가와 왜 사용하는가?)
HTML 파싱은 HTML 문서나 문자열에서 데이터를 추출하는 과정입니다. 프로그래머들은 대개 데이터 스크레이핑, 웹 자동화 혹은 컨텐츠 가공을 목적으로 이를 수행합니다.

## How to: (어떻게 하나요?)
PHP의 `DOMDocument` 클래스를 사용하여 HTML을 파싱하는 기본적인 예제입니다.

```php
<?php
$htmlString = <<<HTML
<!DOCTYPE html>
<html>
<body>
    <h1>Welcome to My Webpage</h1>
    <p>This is a paragraph.</p>
</body>
</html>
HTML;

$dom = new DOMDocument();
libxml_use_internal_errors(true);
$dom->loadHTML($htmlString);
libxml_clear_errors();

$h1 = $dom->getElementsByTagName('h1')->item(0)->nodeValue;
$p = $dom->getElementsByTagName('p')->item(0)->nodeValue;

echo $h1 . "\n" . $p; // 결과 출력
?>
```

실행하면 다음과 같은 출력을 얻을 수 있습니다:

```
Welcome to My Webpage
This is a paragraph.
```

## Deep Dive (깊이 있는 탐구)
HTML 파싱의 역사는 웹의 시작과 함께합니다. 초기에는 정규표현식(regular expressions)이 흔히 사용되었지만, 이 방법은 복잡하고 오류가 많이 발생했습니다. 현대에는 `DOMDocument`와 같이 전용 파서를 사용하며, 이는 정확도가 높고 유연한 처리가 가능합니다.

대안으로는 SimpleXML, PHPQuery, Zend_Dom, Symfony DomCrawler 등의 라이브러리가 있습니다. 각각의 라이브러리는 사용 편의성과 기능에서 차이가 있으며, 상황에 따라 적합한 도구를 선택할 수 있습니다.

구현 세부사항에서 주의해야 할 점은 HTML의 복잡성 때문에 파서는 매우 엄격하지 않도록 설계되어야 합니다. 예를 들어, `DOMDocument`는 `libxml_use_internal_errors(true);`를 활용하여 잘못된 HTML 마크업 처리 시 발생하는 경고나 오류를 무시하도록 할 수 있습니다. 일반적으로 웹에서 가져온 HTML은 완벽하지 않기 때문에, 이러한 관용성은 필수적입니다.

## See Also (관련 자료)
- [DOMDocument](https://www.php.net/manual/en/class.domdocument.php) - PHP 공식 문서 내의 `DOMDocument` 클래스에 대한 설명.
- [PHP SimpleXML](https://www.php.net/manual/en/book.simplexml.php) - 간결한 XML 파싱 및 조작을 위한 PHP의 SimpleXML 확장에 대한 정보.
- [Symfony DomCrawler Component](https://symfony.com/doc/current/components/dom_crawler.html) - Symfony 프레임워크의 DomCrawler 컴포넌트 페이지.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/php+dom) - 개발자 커뮤니티에서 PHP DOM 관련 질문 및 답변을 볼 수 있는 Stack Overflow 태그 페이지.
