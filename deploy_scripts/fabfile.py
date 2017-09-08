from fabric.contrib.files import exists
from fabric.api import env, local, run

REPO_URL = 'https://github.com/alamastor/elm-tetris'


def deploy():
    site_dir = f'~/sites/{env.host}'
    mkdir(site_dir)
    update_source(site_dir)
    make(site_dir)


def mkdir(site_dir):
    if not exists(site_dir):
        run(f'mkdir -p {site_dir}')


def update_source(site_dir):
    if exists(f'{site_dir}/.git'):
        run(f'cd {site_dir} && git fetch')
    else:
        run(f'git clone {REPO_URL} {site_dir}')
    current_commit = local('git log -n 1 --format=%H', capture=True)
    run(f'cd {site_dir} && git reset --hard {current_commit}')


def make(site_dir):
    if not(exists(f'{site_dir}/build')):
        run(f'cd {site_dir} && mkdir build')
    run(f'cd {site_dir} && elm-make src/Main.elm --output=build/index.html')
