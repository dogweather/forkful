---
title:                "टेस्ट लेखन"
html_title:           "Fish Shell: टेस्ट लेखन"
simple_title:         "टेस्ट लेखन"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## Kyu

Tests likhne ka karan hai ki hume apne code ki quality ko improve karne mein madad mil sakti hai aur bugs aur errors ko detect karne mein bhi help karti hai. Tests likhna aapke code ko reliable aur maintainable banata hai.

## Kaise Kare

Fish Shell mein tests likhne ka ek simple tarika hai. Sabse pehle, aapko fish_prompt function mein set_option command use karke **fish_prompt module** ko enable karna hoga. Isse aapke prompt mein modules ka list display hoga.

```Fish Shell
function fish_prompt
	set -l modules (set --query black red blue white)
	set_option --global module_paths $modules

	echo (string join " " $modules)
end
```

Phir aapko connect command use karke fishoscope tool ko install karna hoga. Is tool ki madad se aap apne code ko analyze kar payenge aur tests likh sakte hai.

```Fish Shell
connect fishoscope
```

Fishoscope tool aapke liye ek sample project banayega jisme aap tests likh kar apne code ko analyze kar sakte hai. Aap projects directory mein ja kar apne project ko open kar sakte hai.

```Fish Shell
cd projects
open sample_project
```

Sample project mein 3 files honge - **main.fish**, **functions.fish** aur **test_main.fish**. **main.fish** mein aapka actual code hoga, **functions.fish** mein aapke custom functions honge aur **test_main.fish** mein aapka test code hoga.

```Fish Shell
function rerun
	fish test_main.fish
end
```

Phir aapko **rerun** command use karke apne tests ko run karna hoga.

## Deep Dive

Tests likhna code quality aur reliability ko improve karne ke liye important hai. Aap apne code ko modularize karke tests ka benefit le sakte hai. Modularize karne se aapko apne code ko test karne mein flexibility milti hai aur aap bina kisi dependency ke tests likh sakte hai.

Ek aur tarika tests likhne ka hai **set_unit_test** command ki madad se. Isse aap apne custom functions ko test kar sakte hai.

```Fish Shell
set_unit_test function_example
	type function_example
```

Is tarah aap apne custom functions ko test kar sakte hai.

## Dekho Bhi

- [Fish Shell's official website](https://fishshell.com)
- [Fishoscope tool](https://github.com/kori/fishoscope)