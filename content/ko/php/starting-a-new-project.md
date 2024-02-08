---
title:                "새 프로젝트 시작하기"
aliases:
- ko/php/starting-a-new-project.md
date:                  2024-01-20T18:03:59.102188-07:00
model:                 gpt-4-1106-preview
simple_title:         "새 프로젝트 시작하기"

tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/php/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (무엇을 위한 것이며, 왜?)
새 프로젝트를 시작한다는 건, 공백에서 새로운 아이디어를 현실로 만드는 과정입니다. 프로그래머들은 문제를 해결하거나 뭔가 새로운 것을 창조하기 위해 이 과정을 반복합니다.

## How to: (어떻게:)
```PHP
<?php
// PHP 프로젝트 시작하기
// 프로젝트 폴더 생성
mkdir("my_new_project");

// 기본 index.php 파일 생성
file_put_contents("my_new_project/index.php", "<?php\n// 여기서부터 코드를 작성하세요.\n");

echo "새 프로젝트가 성공적으로 생성되었습니다.";

// Sample output: 새 프로젝트가 성공적으로 생성되었습니다.
```

## Deep Dive (깊이 알아보기)
새로운 PHP 프로젝트를 시작하는 것은 간단해 보일 수 있지만, 좋은 초석을 놓는 것이 중요합니다. 과거에는 모든 것을 수동으로 설정해야 했지만, 오늘날에는 Composer와 같은 도구를 사용하여 의존성 관리를 하고, PSR 표준에 맞춰 코드를 작성합니다. 프로젝트의 성격에 따라 단순히 파일을 만드는 것 이상의 준비가 필요할 수 있습니다. 예를 들어, MVC 구조를 사용할 계획이라면, 이에 맞는 디렉토리 구조를 세심하게 고려해야 합니다.

## See Also (관련 자료)
- PHP 공식 매뉴얼: https://www.php.net/manual/en/
- Composer 홈페이지: https://getcomposer.org/
- PHP-FIG(PSR 표준): https://www.php-fig.org/psr/ 

이러한 자료들은 새 프로젝트를 시작할 때 유용한 정보와 통찰력을 제공합니다.
