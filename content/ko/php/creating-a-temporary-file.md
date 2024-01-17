---
title:                "임시 파일 생성"
html_title:           "PHP: 임시 파일 생성"
simple_title:         "임시 파일 생성"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?
임시 파일 생성은 프로그래머들이 일시적으로 사용할 파일을 만드는 것을 말합니다. 이 파일들은 프로그램의 실행에 필요한 중간 결과물을 저장하거나 다른 프로그램과의 통신을 위해 사용될 수 있습니다.

## 실습:
```PHP
// PHP에서 임시 파일 생성하기
$filename = tempnam(sys_get_temp_dir(), 'temp-');
echo $filename;
// 예상 출력: /tmp/temp-12345678
```

## 깊은 이해:
1. 임시 파일 생성은 이전에는 메모리 낭비가 발생했던 프로그램 수행 중의 중간 결과물을 저장하는데 사용되었습니다. 하지만 지금은 메모리 관리 기술이 발전하여 임시 파일 생성이 그다지 필요하지 않을 수도 있습니다.
2. 대체로 임시 파일 생성은 메모리보다는 디스크 공간을 차지하기 때문에 메모리 사용에 제한이 있는 환경에서 유용합니다.
3. 임시 파일 생성시 생성되는 이름은 임시 디렉토리 경로와 지정한 이름에 무작위 숫자가 붙는 형식을 가지고 있습니다.

## 연관 자료:
- PHP 공식 문서: https://www.php.net/manual/en/function.tempnam.php
- 임시 파일 생성의 사용 사례: https://stackoverflow.com/questions/193459/what-are-the-best-practices-for-using-a-tmp-directory-for-file-uploads-in-php