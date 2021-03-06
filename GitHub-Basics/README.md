# Git tutorial

## What is git? (and why you should be using it for practically everything)

![git](https://imgs.xkcd.com/comics/git.png)

[Image source](https://xkcd.com/1597/)

Git is a *free* version control system which helps you keep track of file changes in your computer. 
Think of it as a time machine that lets you go back to any point in your project development.

While git is most used in software development, you can use it for anything you like ([writing 
books](https://www.gitbook.com/), for example), as long as your files are plain text (e.g. source 
code, latex files), you won't have any issue with git (this guide is actually hosted using git, 
git-ception!).

Simply speaking, git saves snapshots of your work called `commits`. After a `commit` is made, you 
can go back and forth to check the state of your project. Maybe you were experimenting with some 
new function and realized the old one was better. No problem, you can bring back anything!

![git 2](https://imgs.xkcd.com/comics/git_commit.png)

[Image source](https://xkcd.com/1296/)

The entire development of your project is stored in your computer, but we know that's dangerous, so 
you can also host a remote copy (just like you do with Dropbox or Google Drive).

## What is GitHub?

There are [many](https://gitlab.com/), [many](https://bitbucket.org/) providers that let you store 
your git repositories (that's how you call a *git project*) but the most widely used is 
[GitHub](https://github.com/)

Apart from storing a copy of your projects, GitHub comes with a lot of useful features. For 
example, you can use it to share your projects with your colleagues, so they can see (or modify) your project.


## git sounds awesome! How do I get it?

Chances are, git is already installed on your computer. If not, you can get it from 
[here](https://git-scm.com/).

OS X users: use homebrew

## Can I get buttons and stuff?

git is a command line tool, which means it doesn't have a graphical user interface. GitHub Desktop came out recently which provides a GUI. But, using the git command line interface is the most flexible way of working with git. 

However, if you still want a GUI (e.g. for using git in your computer), here are some options 
available:

*   [GitHub Desktop] (https://desktop.github.com/)
*   [Options for Mac](https://git-scm.com/download/gui/mac)
*   [GitKraken](https://www.gitkraken.com/) (Windows and Mac)

## Ok, how do I do the magic?

### Resources for beginners

* [15 minute tutorial to learn git](https://try.github.io/levels/1/challenges/1) - This is a must 
for people to get started.
* [git - the simple guide](http://rogerdudler.github.io/git-guide/) - A simple guide to get to know 
the most important concepts.

### Resouces for becoming a git ninja

* [A successful git branching model](http://nvie.com/posts/a-successful-git-branching-model/) - A 
model to work with git using branches. This model is widely used in the open source community.
* [Learn Git Branching](http://learngitbranching.js.org/) - Understanding what branches and rebases 
are, in an amazing interactive tutorial.
* [Reset Demystified](https://git-scm.com/blog/2011/07/11/reset.html) - A blog post on `git reset` 
which develops some useful concepts along the way.
* [Understanding git for real by exploring the .git directory](https://medium.freecodecamp.com/understanding-git-for-real-by-exploring-the-git-directory-1e079c15b807#.5pe75gc07) - A blog post on what's inside a commit.
* [A git style guide](https://github.com/agis/git-style-guide), complete with branch naming, suggestions on how to handle commit messages, and more.


## READ THIS BEFORE YOU LEAVE  (please)

By default, git saves *everything* inside the folder where you initiated the repo. When working on 
software projects there are files you DON'T want to save on git (e.g. database passwords, 
especially if you have a remote copy).

To prevent git from saving files, create a file and name it `.gitignore` in the folder where you 
ran `git init`. In such file, you can add rules to let git know what you want it to ignore. All you need to do is specify the file name or file extension type. For more information, [read this](https://git-scm.com/docs/gitignore).

Besides sensitive data, you want to also ignore intermediate files generated automatically by some 
programming languages or libraries (e.g., .Rproj files from R Studio projects). There are [templates](https://github.com/github/gitignore) 
available depending on the tools you use. There's also a nice [command line 
tool](https://github.com/karan/joe) to fetch such templates.
