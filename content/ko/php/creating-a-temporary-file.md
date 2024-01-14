---
title:    "PHP: 임시 파일 생성하기"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜?

임시 파일 생성을 시작하는 이유는 다양합니다. 예를 들어, 웹 개발에서는 사용자가 업로드 한 파일을 처리하기 전에 임시 파일로 저장하는 것이 일반적입니다.

## 어떻게?

```PHP
$temp = tempnam(sys_get_temp_dir(), 'prefix_'); //임시 파일 생성
echo $temp; //임시 파일 경로 출력
```

위의 코드는 PHP의 `tempnam` 함수를 사용하여 임시 파일을 생성하는 간단한 예시입니다. `sys_get_temp_dir` 함수는 운영체제의 기본 임시 파일 디렉토리를 반환하며, `prefix_`는 파일 이름에 붙일 접두사를 지정하는 파라미터입니다.

출력 예시: `C:\Windows\temp\prefix_uaxD7s`

## 깊게 파고들기

임시 파일을 생성하는 방법은 환경에 따라 다를 수 있습니다. 일반적으로는 `tempnam` 함수가 가장 간단한 방법입니다. 그러나 PHP의 `tmpfile` 함수를 사용하면 파일 포인터를 반환할 수 있어 파일을 쓰고 읽는 작업에 더 편리합니다. 또한 임시 파일 생성 시에 파일 이름에 랜덤 값을 추가하여 보안을 높이는 것도 중요합니다.

## 자세히 알아보기

임시 파일 생성은 자주 사용되는 기술 중 하나이지만, 올바르게 구현하지 않으면 중요한 데이터 손실이나 보안 문제를 초래할 수 있습니다. 따라서 임시 파일을 생성할 때에는 반드시 정확한 방법과 올바른 파라미터를 사용하는 것이 중요합니다. 또한 파일을 삭제하지 않고 남겨두면 보안 문제를 초래할 수 있으므로 임시 파일을 사용한 뒤에는 적절하게 삭제하는 것이 좋습니다.

## 관련 링크

- [PHP 매뉴얼 - 임시 파일 생성](http://php.net/manual/ko/function.tempnam.php)
- [쿠키파이 - PHP로 임시 파일 생성하기](https://www.cookycookie.kr/102)
- [제로보드 - 임시 파일 생성 관련 보안 문제](https://zbob.kr/zbdumb/view.php?id=tip_tech&no=137)