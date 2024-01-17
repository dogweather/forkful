---
title:                "웹 페이지 다운로드하기"
html_title:           "PHP: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 뭐고 왜해? 
웹 페이지를 다운로드한다는 것은 프로그래머가 웹 페이지의 소스 코드를 컴퓨터로 저장하는 것을 의미합니다. 프로그래머는 이를 통해 웹 페이지의 정보를 사용할 수 있게 됩니다.

## 방법: 
아래의 예시 코드를 통해 다운로드하는 방법을 알아보겠습니다.

```PHP
<?php
$url = "https://www.example.com/index.html";
$contents = file_get_contents($url);
echo $contents;
?>
```
위 코드는 "www.example.com"의 인덱스 페이지를 다운로드하고, 컴퓨터에 저장한 후 해당 페이지의 소스 코드를 출력합니다.

## 깊이 파헤치기: 
우리는 더 이상 웹 페이지를 수동으로 다운로드하고 저장해야 하는 시대를 지나서, PHP를 통해 간단하게 웹 페이지를 다운로드할 수 있게 되었습니다. 또한, file_get_contents() 함수 외에도 cURL 등 다른 방법으로도 웹 페이지를 다운로드할 수 있습니다.

## 관련 정보: 
추가적인 정보는 아래의 링크를 참고해주세요.
- [PHP 공식 문서](https://www.php.net/manual/en/function.file-get-contents.php)
- [cURL 사용 예시](https://www.geeksforgeeks.org/how-to-use-curl-in-php/)
- [웹 크롤링과 관련된 더 많은 정보](https://medium.com/@ieuneo/web-crawling-%EB%A7%88%EC%86%8C%EC%9D%84-%ED%86%B5%ED%95%B4%ED%95%98%EC%A7%80-%EC%95%8A%EC%95%98%EC%8B%9C%EB%8A%94-%EB%B2%95-fa77f2fc4411)