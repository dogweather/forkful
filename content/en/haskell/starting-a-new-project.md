---
title:                "Starting a new project"
date:                  2024-01-20T18:03:42.182088-07:00
model:                 gpt-4-1106-preview
simple_title:         "Starting a new project"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/starting-a-new-project.md"
changelog:
  - dogweather, 2024-01-21
---

{{< edit_this_page >}}

## What & Why?
Every project starts with a single step. For programmers, that means setting up the initial structure and writing kick-off code. We do this to transform ideas into a concrete foundation, ready for expansion and innovation.

## How to:
```Haskell
-- 1. Initializing a new Haskell project using Stack
$ stack new myproject

-- The above command creates a new directory `myproject` with some files:
-- myproject/
-- ├── app/
-- │   └── Main.hs        # Your Main application file
-- ├── src/               # Source files for the library
-- ├── test/              # Test files
-- ├── myproject.cabal    # Package description file
-- ├── stack.yaml         # Stack configuration
-- └── Setup.hs           # Build setup script

-- 2. Building the project
$ cd myproject
$ stack build

-- 3. Running your new Haskell project
$ stack run

-- Sample output:
someFunc
```

## Deep Dive
Haskell projects often rely on tools like Stack or Cabal. Stack manages dependencies, ensuring consistent builds. In 2008, Stack was a game-changer for Haskell, addressing Cabal's shortcomings with package conflicts. 

Alternatives include using Cabal alone or newer tools like GHCup or Nix for reproducible builds. You might choose Cabal for simplicity or Nix when your work demands reproducibility, but Stack strikes a happy balance for many.

Under the hood, `stack new` leverages a template to scaffold a project. It includes not just your source code but also configurations for building and dependencies. The `.cabal` file is pivotal, containing metadata and build instructions.

## See Also
- Learn more about Stack: [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)
- Dive into Cabal: [The Haskell Cabal](https://www.haskell.org/cabal/users-guide/)
