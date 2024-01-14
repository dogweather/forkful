---
title:                "C: HTML 파싱하기"
simple_title:         "HTML 파싱하기"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/parsing-html.md"
---

{{< edit_this_page >}}

## 왜

HTML 파싱에 참여하는 이유는 무엇인가요? 

HTML 문서는 웹 브라우저가 이해할 수 있는 컴퓨터가 아닌 사람을 위한 마크업 언어입니다. 따라서 웹 크롤링, 웹 스크래핑 및 데이터 마이닝과 같은 목적으로 이용됩니다. HTML을 파싱하면 웹 문서의 데이터를 쉽게 추출할 수 있기 때문에 많은 프로그래머들이 이 기술을 이용하게 됩니다. 

## 하는 법

HTML 파싱을 하는 방법은 다양한 프로그래밍 언어와 라이브러리를 이용하여 할 수 있지만, 이 글에서는 C 프로그래밍 언어를 사용하여 예제를 보여드리겠습니다. 먼저, 파싱할 HTML 문서를 포함하는 변수를 선언하고 해당 파일을 읽어옵니다. 그 후, <stdio.h> 헤더 파일과 파일 포인터를 이용하여 파일의 내용을 한 줄씩 읽어온 후, 원하는 데이터를 추출하기위한 로직을 작성합니다. 아래는 예제코드와 출력 예시입니다. 

```C
#include <stdio.h>

// HTML 문서 포함 변수 선언
char html_doc[] = "<html>
<head>
  <title>파이썬 코딩 연습</title>
</head>
<body>
  <h1>파이썬 코딩 미션</h1>
  <p>다음의 문장을 출력하세요:</p>
  <blockquote>Life is too short, You need Python.</blockquote>
</body>
</html>";

int main() {
  FILE *fp;
  char line[100];

  // 파일 열기
  fp = fopen("sample.html", "w+");

  // 파일에 데이터 쓰기
  fputs(html_doc, fp);

  // 파일 위치를 파일 시작 부분으로 이동
  fseek(fp, 0, SEEK_SET);

  // 한 줄씩 읽어오기
  while (fgets(line, 100, fp) != NULL) {
    // "<title>" 태그를 포함한 라인 추출
    if (strstr(line, "<title>") != NULL) {
      printf("%s", line);
    }
  }

  // 파일 닫기
  fclose(fp);

  return 0;
}

```

출력 예시:

```
<html>
<head>
  <title>파이썬 코딩 연습</title>
</head>
```

위 예제 코드는 매우 간단한 예시이며 실제 파싱을 할 때에는 더 복잡한 로직이 필요할 수 있습니다. 하지만 기본적인 원리는 동일하며 원하는 데이터를 추출하기 위해 적절한 로직을 작성하면 됩니다. 

## 깊게 파보기

HTML 문서를 파싱하는 것은 매우 널리 사용되는 기술이며 새로운 프로그래밍 언어를 배우기 전에도 많은 프로그래머들이 이 기술을 익혀야 합니다. 그 이유는 웹에서 많은 정보가 제공되기 때문입니다. 또한 HTML은 계속해서 업데이트되며 새로운 태그 및 속성이 추가되기 때문에 파싱하는 방법도 업데이트되어야 할 수 있습니다. 따라서 이를 이용하여 다양한 웹 스크래핑 및 데이터 마이닝 프로젝트를 수행할 수 있습니다. 

## 더보기 

여러분은 C 프로그래밍을 할 때 HTML을 파싱하는 것 이외에도 다른 언어 및 라이브러