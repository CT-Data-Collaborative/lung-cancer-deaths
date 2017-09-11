import pytest
import datapackage



@pytest.fixture
def towns():
    """Load our town list datapackage"""
    dp = datapackage.DataPackage('https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json')
    return dp.resources[0].data


